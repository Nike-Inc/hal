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
import           Data.Aeson               (Value, decode)
import           Data.Aeson.Types         (FromJSON)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy     as BSW
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Network.HTTP.Client      (Response, responseBody,
                                           responseHeaders)
import           Network.HTTP.Types       (HeaderName)

eventResponseToNextData :: StaticContext -> Response Value -> (BS.ByteString, Value, Either String LambdaContext)
eventResponseToNextData staticContext nextRes =
  -- If we got an event but our requestId is invalid/missing, there's no hope of meaningful recovery
  let reqIdBS = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

      mTraceId = fmap decodeUtf8 $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
      mFunctionArn = fmap decodeUtf8 $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Invoked-Function-Arn" nextRes
      mDeadline = do
        header <- exactlyOneHeader (getResponseHeader "Lambda-Runtime-Deadline-Ms" nextRes)
        milliseconds :: Double <- readMaybe $ BSC.unpack header
        return $ posixSecondsToUTCTime $ realToFrac $ milliseconds / 1000

      mClientContext = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Client-Context" nextRes
      mIdentity = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Cognito-Identity" nextRes

      -- Build out the Dynamic portion of the Lambda Context
      eDynCtx =
        maybeToEither "Runtime Error: Unable to decode Context from event response."
        -- Build the Dynamic Context, collapsing individual Maybes into a single Maybe
        $ DynamicContext (decodeUtf8 reqIdBS)
        <$> mFunctionArn
        <*> mTraceId
        <*> mDeadline
        <*> mClientContext
        <*> mIdentity

      -- combine our StaticContext and possible DynamicContext into a LambdaContext
      eCtx = fmap (mkContext staticContext) eDynCtx

      event = getResponseBody nextRes

  -- Return the interesting components
  in (reqIdBS, event, eCtx)


-- Helpers (mostly) for Headers

getResponseBody :: Response a -> a
getResponseBody = responseBody

getResponseHeader :: HeaderName -> Response a -> [BS.ByteString]
getResponseHeader headerName = fmap snd . filter ((==) headerName . fst) . responseHeaders

exactlyOneHeader :: [a] -> Maybe a
exactlyOneHeader [a] = Just a
exactlyOneHeader _ = Nothing

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
decodeHeaderValue :: FromJSON a => BSC.ByteString -> Maybe a
decodeHeaderValue = decode . BSW.pack . fmap BSI.c2w . BSC.unpack

-- An empty array means we successfully decoded, but nothing was there
-- If we have exactly one element, our outer maybe signals successful decode,
--   and our inner maybe signals that there was content sent
-- If we had more than one header value, the event was invalid
decodeOptionalHeader :: FromJSON a => [BSC.ByteString] -> Maybe (Maybe a)
decodeOptionalHeader header =
  case header of
    [] -> Just Nothing
    [x] -> fmap Just $ decodeHeaderValue x
    _ -> Nothing
