{-# LANGUAGE CPP #-}
{-|
Module      : AWS.Lambda.RuntimeClient.Internal
Description : Internal HTTP related machinery for talking to the AWS Lambda Custom Runtime interface.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : unstable
-}

module AWS.Lambda.RuntimeClient.Internal (
  NextData(..),
  eventResponseToNextData,
  generateRootTraceId,
  mkRootTraceId,
) where

import           AWS.Lambda.Context       (LambdaContext)
import           AWS.Lambda.Internal      (DynamicContext (..), StaticContext,
                                           mkContext)
import           Data.Aeson               (Value, eitherDecode)
import           Data.Aeson.Types         (FromJSON)
import           Data.Bifunctor           (first)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy     as BSW
import           Data.CaseInsensitive     (original)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup           ((<>))
#endif
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock.POSIX    (POSIXTime, getPOSIXTime,
                                           posixSecondsToUTCTime)
import           Data.Word                (Word32, Word64)
import           Network.HTTP.Client      (Response, responseBody,
                                           responseHeaders)
import           Network.HTTP.Types       (HeaderName, ResponseHeaders)
import           Numeric                  (showHex)
import           System.Random            (randomIO)

-- | What the runtime takes from a "next invocation" response.
--
-- The IDs and the event are strict: the runtime needs them on
-- every invocation, and forcing them up front means nothing here keeps the
-- response itself alive.  The context is deliberately lazy, since only some
-- handlers look at it, and it is built from the response headers alone, so
-- holding on to it does not hold on to the event.
data NextData = NextData
  { nextRequestId    :: !BS.ByteString
    -- | Echoed back with the response or error, so that Lambda can reject one
    -- from an earlier attempt at the same request.  'Nothing' if the response
    -- had none, or more than one.
    --
    -- InvocationId only helps for async invocations in Managed Instances,
    -- where it disambiguates responses across retries.  For sync invocations,
    -- events are never retried, so there's at most one response.  For async
    -- invocations in the regular lambda environment, the lambda won't retry
    -- after a timeout until it knows that the runtime is cleaned up, which
    -- means there can be at most one attempt in flight at once.  However, in
    -- Managed Instances, this approach isn't realistic: the runtime may be
    -- handling many concurrent requests, and shutting it down aborts them.
    -- Since it doesn't kill the runtime, it's possible that the timed out
    -- request does eventually respond--possibly before a retried attempt does.
    -- This id allows detection of a timed out async retry, discarding it
    -- instead of confusing it for the current attempt.
  , nextInvocationId :: !(Maybe BS.ByteString)
    -- | The trace header's value, or the fallback if the response had none.
    -- 'Nothing' only if the response had more than one trace header.
  , nextTraceId      :: !(Maybe Text)
  , nextEvent        :: !Value
  , nextContext      :: Either String LambdaContext
  } deriving (Show, Eq)

-- | The 'Text' argument is the trace id to use if the response has no
-- @Lambda-Runtime-Trace-Id@ header (see 'generateRootTraceId').
eventResponseToNextData :: StaticContext -> Text -> Response Value -> NextData
eventResponseToNextData staticContext fallbackTraceId nextRes =
  let
    -- Everything below works from the headers, never from the response.
    !nextResHeaders = responseHeaders nextRes

    -- If we got an event but our requestId is invalid/missing, there's no hope of meaningful recovery
    reqIdBS = either (error . ("Runtime Error: " <>)) id $
      exactlyOneHeader "Lambda-Runtime-Aws-Request-Id" nextResHeaders

    -- Lambda accepts a response or error without the invocation ID, so if the
    -- header is repeated, we leave it out rather than guess.
    invocationId = either (const Nothing) id $
      atMostOneHeader "Lambda-Runtime-Invocation-Id" nextResHeaders

    -- The trace header is omitted when there is no trace to propagate (for
    -- example, under the Lambda Runtime Interface Emulator).  There is
    -- nothing to connect to in that case, so we start a new root trace.
    --
    -- Read on its own, so that tracing does not depend on the rest of the
    -- context being decodable.
    eTraceId = maybe fallbackTraceId decodeUtf8 <$> atMostOneHeader "Lambda-Runtime-Trace-Id" nextResHeaders

    eCtx = first ("Runtime Error: Unable to decode Context from event response.\n" <>) $ do
      traceId <- eTraceId
      functionArn <- fmap decodeUtf8 $ exactlyOneHeader "Lambda-Runtime-Invoked-Function-Arn" nextResHeaders
      deadlineHeader <- exactlyOneHeader "Lambda-Runtime-Deadline-Ms" nextResHeaders
      milliseconds :: Double <- maybeToEither "Could not parse deadline" $ readMaybe $ BSC.unpack deadlineHeader
      let deadline = posixSecondsToUTCTime $ realToFrac $ milliseconds / 1000

      clientContext <- decodeOptionalHeader "Lambda-Runtime-Client-Context" nextResHeaders
      identity <- decodeOptionalHeader "Lambda-Runtime-Cognito-Identity" nextResHeaders

      -- Build out the Dynamic portion of the Lambda Context
      let dynCtx = DynamicContext (decodeUtf8 reqIdBS) functionArn traceId deadline clientContext identity

      -- combine our StaticContext and possible DynamicContext into a LambdaContext
      return (mkContext staticContext dynCtx)

  -- Return the interesting components
  in NextData
       { nextRequestId = reqIdBS
       , nextInvocationId = invocationId
       , nextTraceId = either (const Nothing) Just eTraceId
       , nextEvent = getResponseBody nextRes
       , nextContext = eCtx
       }


-- | Start a new, unsampled X-Ray root trace, formatted as a
-- @Lambda-Runtime-Trace-Id@ header value.
generateRootTraceId :: IO Text
generateRootTraceId = mkRootTraceId <$> getPOSIXTime <*> randomIO <*> randomIO

-- | Format a root trace id from its start time and the two halves of its
-- 96-bit identifier, e.g. @Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=0@.
--
-- See <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids>
mkRootTraceId :: POSIXTime -> Word32 -> Word64 -> Text
mkRootTraceId time idHigh idLow =
  T.pack $ "Root=1-" <> hexPadded 8 (toInteger epochSeconds) <> "-" <> hexPadded 8 (toInteger idHigh) <> hexPadded 16 (toInteger idLow) <> ";Sampled=0"
  where
    -- Truncated to 32 bits, the format's fixed width
    epochSeconds = fromIntegral (floor time :: Integer) :: Word32
    -- Monomorphic, since showHex needs a Show constraint on older bases
    hexPadded :: Int -> Integer -> String
    hexPadded width n = let digits = showHex n "" in replicate (width - length digits) '0' <> digits

-- Helpers (mostly) for Headers

getResponseBody :: Response a -> a
getResponseBody = responseBody

getResponseHeader :: HeaderName -> ResponseHeaders -> [BS.ByteString]
getResponseHeader headerName = fmap snd . filter ((==) headerName . fst)

headerNameToString :: HeaderName -> String
headerNameToString = fmap BSI.w2c . BS.unpack . original

exactlyOneHeader :: HeaderName -> ResponseHeaders -> Either String BS.ByteString
exactlyOneHeader name headers =
  let nameStr = headerNameToString name
  in case getResponseHeader name headers of
    [a] -> Right a
    [] -> Left ("Missing response header " <> nameStr)
    _ ->  Left ("Too many values for header " <> nameStr)

-- Like exactlyOneHeader, but the header may be absent
atMostOneHeader :: HeaderName -> ResponseHeaders -> Either String (Maybe BS.ByteString)
atMostOneHeader name headers =
  let nameStr = headerNameToString name
  in case getResponseHeader name headers of
    [a] -> Right (Just a)
    [] -> Right Nothing
    _ ->  Left ("Too many values for header " <> nameStr)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b ma = case ma of
  Nothing -> Left b
  Just a -> Right a

-- Note: Does not allow whitespace
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x,"")] -> Just x
  _ -> Nothing

-- TODO: There must be a better way to do this
decodeHeaderValue :: FromJSON a => BSC.ByteString -> Either String a
decodeHeaderValue = eitherDecode . BSW.pack . fmap BSI.c2w . BSC.unpack

-- An empty array means we successfully decoded, but nothing was there
-- If we have exactly one element, our outer maybe signals successful decode,
--   and our inner maybe signals that there was content sent
-- If we had more than one header value, the event was invalid
decodeOptionalHeader :: FromJSON a => HeaderName -> ResponseHeaders -> Either String (Maybe a)
decodeOptionalHeader name headers =
  let nameStr = headerNameToString name
  in case getResponseHeader name headers of
    [] -> Right Nothing
    [x] -> first (\e -> "Could not JSON decode header " <> nameStr <> ": " <> e) $ fmap Just $ decodeHeaderValue x
    _ -> Left ("Too many values for header " <> nameStr)
