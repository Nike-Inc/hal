{-# LANGUAGE CPP #-}
{-|
Module      : AWS.Lambda.RuntimeClient.Internal
Description : Internal HTTP related machinery for talking to the AWS Lambda Custom Runtime interface.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.RuntimeClient.Internal (
  eventResponseToNextData,
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
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Network.HTTP.Client      (Response, responseBody,
                                           responseHeaders)
import           Network.HTTP.Types       (HeaderName)

eventResponseToNextData :: StaticContext -> Response Value -> (BS.ByteString, Value, Either String LambdaContext)
eventResponseToNextData staticContext nextRes =
  -- If we got an event but our requestId is invalid/missing, there's no hope of meaningful recovery
  let
    reqIdBS = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

    eCtx = first ("Runtime Error: Unable to decode Context from event response.\n" <>) $ do
      traceId <- fmap decodeUtf8 $ exactlyOneHeader "Lambda-Runtime-Trace-Id" nextRes
      functionArn <- fmap decodeUtf8 $ exactlyOneHeader "Lambda-Runtime-Invoked-Function-Arn" nextRes
      deadlineHeader <- exactlyOneHeader "Lambda-Runtime-Deadline-Ms" nextRes
      milliseconds :: Double <- maybeToEither "Could not parse deadline" $ readMaybe $ BSC.unpack deadlineHeader
      let deadline = posixSecondsToUTCTime $ realToFrac $ milliseconds / 1000

      clientContext <- decodeOptionalHeader "Lambda-Runtime-Client-Context" nextRes
      identity <- decodeOptionalHeader "Lambda-Runtime-Cognito-Identity" nextRes

      -- Build out the Dynamic portion of the Lambda Context
      let dynCtx = DynamicContext (decodeUtf8 reqIdBS) functionArn traceId deadline clientContext identity

      -- combine our StaticContext and possible DynamicContext into a LambdaContext
      return (mkContext staticContext dynCtx)

  -- Return the interesting components
  in (reqIdBS, getResponseBody nextRes, eCtx)


-- Helpers (mostly) for Headers

getResponseBody :: Response a -> a
getResponseBody = responseBody

getResponseHeader :: HeaderName -> Response a -> [BS.ByteString]
getResponseHeader headerName = fmap snd . filter ((==) headerName . fst) . responseHeaders

headerNameToString :: HeaderName -> String
headerNameToString = fmap BSI.w2c . BS.unpack . original

exactlyOneHeader :: HeaderName -> Response Value -> Either String BS.ByteString
exactlyOneHeader name res =
  let nameStr = headerNameToString name
  in case getResponseHeader name res of
    [a] -> Right a
    [] -> Left ("Missing response header " <> nameStr)
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
decodeOptionalHeader :: FromJSON a => HeaderName -> Response Value -> Either String (Maybe a)
decodeOptionalHeader name res =
  let nameStr = headerNameToString name
  in case getResponseHeader name res of
    [] -> Right Nothing
    [x] -> first (\e -> "Could not JSON decode header " <> nameStr <> ": " <> e) $ fmap Just $ decodeHeaderValue x
    _ -> Left ("Too many values for header " <> nameStr)
