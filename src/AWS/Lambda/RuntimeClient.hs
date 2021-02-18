{-|
Module      : AWS.Lambda.RuntimeClient
Description : HTTP related machinery for talking to the AWS Lambda Custom Runtime interface.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.RuntimeClient (
  RuntimeClientConfig,
  getRuntimeClientConfig,
  getNextData,
  getNextEvent,
  sendEventSuccess,
  sendEventError,
) where

import           AWS.Lambda.Context        (LambdaContext(..))
import           AWS.Lambda.Internal       (DynamicContext(..), StaticContext,
                                            mkContext)
import           Control.Applicative       ((<*>))
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (displayException, try, throw)
import           Control.Monad             (unless)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (decode, encode, Value)
import           Data.Aeson.Parser         (value')
import           Data.Aeson.Types          (FromJSON, ToJSON)
import           Data.Bifunctor            (first)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as BSW
import qualified Data.ByteString.Internal  as BSI
import           Data.Conduit              (ConduitM, runConduit, yield, (.|))
import           Data.Conduit.Attoparsec   (sinkParser)
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Time.Clock.POSIX     (posixSecondsToUTCTime)
import           GHC.Generics              (Generic (..))
import           Network.HTTP.Client       (BodyReader, HttpException, Manager,
                                            Request,
                                            RequestBody (RequestBodyLBS),
                                            Response, brRead,
                                            defaultManagerSettings, httpNoBody,
                                            managerConnCount,
                                            managerIdleConnectionCount,
                                            managerResponseTimeout,
                                            managerSetProxy, method, newManager,
                                            noProxy, parseRequest, path,
                                            requestBody, requestHeaders,
                                            responseBody, responseHeaders,
                                            responseStatus, responseTimeoutNone,
                                            setRequestCheckStatus, withResponse)
import           Network.HTTP.Types        (HeaderName)
import           Network.HTTP.Types.Status (Status, status403, status413,
                                            statusIsSuccessful)
import           System.Environment        (getEnv)
import           System.Envy               (decodeEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

data RuntimeClientConfig = RuntimeClientConfig Request Manager StaticContext

-- Exposed Handlers

-- TODO: It would be interesting if we could make the interface a sort of
-- "chained" callback API.  So instead of getting back a base request to kick
-- things off we get a 'getNextEvent' handler and then the 'getNextEvent'
-- handler returns both the 'success' and 'error' handlers.  So things like
-- baseRequest and reqId are pre-injected.
getRuntimeClientConfig :: IO RuntimeClientConfig
getRuntimeClientConfig = do
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  req <- parseRequest $ "http://" ++ awsLambdaRuntimeApi
  man <- newManager
           -- In the off chance that they set a proxy value, we don't want to
           -- use it.  There's also no reason to spend time reading env vars.
           $ managerSetProxy noProxy
           $ defaultManagerSettings
             -- This is the most important setting, we must not timeout requests
             { managerResponseTimeout = responseTimeoutNone
             -- We only ever need a single connection, because we'll never make
             -- concurrent requests and never talk to more than one host.
             , managerConnCount = 1
             , managerIdleConnectionCount = 1
             }

  possibleStaticCtx <- liftIO $ (decodeEnv :: IO (Either String StaticContext))

  case possibleStaticCtx of
    Left err -> do
      liftIO $ sendInitError req man err
      error err
    Right staticCtx -> return $ RuntimeClientConfig req man staticCtx


getNextData :: RuntimeClientConfig -> IO (BS.ByteString, Value, Either String LambdaContext)
getNextData runtimeClientConfig@(RuntimeClientConfig _ _ staticContext) = do
  nextRes <- getNextEvent runtimeClientConfig

  -- If we got an event but our requestId is invalid/missing, there's no hope of meaningful recovery
  let reqIdBS = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  let mTraceId = fmap decodeUtf8 $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  let mFunctionArn = fmap decodeUtf8 $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Invoked-Function-Arn" nextRes
  let mDeadline = do
        header <- exactlyOneHeader (getResponseHeader "Lambda-Runtime-Deadline-Ms" nextRes)
        milliseconds :: Double <- readMaybe $ BSC.unpack header
        return $ posixSecondsToUTCTime $ realToFrac $ milliseconds / 1000

  let mClientContext = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Client-Context" nextRes
  let mIdentity = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Cognito-Identity" nextRes

  -- Build out the Dynamic portion of the Lambda Context
  let eDynCtx =
        maybeToEither "Runtime Error: Unable to decode Context from event response."
        -- Build the Dynamic Context, collapsing individual Maybes into a single Maybe
        $ DynamicContext (decodeUtf8 reqIdBS)
        <$> mFunctionArn
        <*> mTraceId
        <*> mDeadline
        <*> mClientContext
        <*> mIdentity

  -- combine our StaticContext and possible DynamicContext into a LambdaContext
  let eCtx = fmap (mkContext staticContext) eDynCtx

  let event = getResponseBody nextRes

  -- Return the interesting components
  return (reqIdBS, event, eCtx)

-- AWS lambda guarantees that we will get valid JSON,
-- so parsing is guaranteed to succeed.
getNextEvent :: RuntimeClientConfig -> IO (Response Value)
getNextEvent (RuntimeClientConfig baseRuntimeRequest manager _) = do
  resOrEx <- runtimeClientRetryTry $ flip httpValue manager $ toNextEventRequest baseRuntimeRequest
  let checkStatus res = if not $ statusIsSuccessful $ getResponseStatus res then
        Left "Unexpected Runtime Error:  Could not retrieve next event."
      else
        Right res
  let resOrMsg = first (displayException :: HttpException -> String) resOrEx >>= checkStatus
  case resOrMsg of
    Left msg -> do
      _ <- sendInitError baseRuntimeRequest manager msg
      error msg
    Right y -> return y

sendEventSuccess :: ToJSON a => RuntimeClientConfig -> BS.ByteString -> a -> IO ()
sendEventSuccess rcc@(RuntimeClientConfig baseRuntimeRequest manager _) reqId json = do
  resOrEx <- runtimeClientRetryTry $ flip httpNoBody manager $ toEventSuccessRequest reqId json baseRuntimeRequest

  let resOrTypedMsg = case resOrEx of
        Left ex ->
          -- aka NonRecoverable
          Left $ Left $ displayException (ex :: HttpException)
        Right res ->
          if getResponseStatus res == status413 then
            -- TODO Get the real error info from the response
            -- aka Recoverable
            Left (Right "Payload Too Large")
          else if not $ statusIsSuccessful $ getResponseStatus res then
            --aka NonRecoverable
            Left (Left "Unexpected Runtime Error: Could not post handler result.")
          else
            --aka Success
            Right ()

  case resOrTypedMsg of
    Left (Left msg) ->
      -- If an exception occurs here, we want that to propogate
      sendEventError rcc reqId msg
    Left (Right msg) -> error msg
    Right () -> return ()

sendEventError :: RuntimeClientConfig -> BS.ByteString -> String -> IO ()
sendEventError (RuntimeClientConfig baseRuntimeRequest manager _) reqId e =
  fmap (const ()) $ runtimeClientRetry $ flip httpNoBody manager $ toEventErrorRequest reqId e baseRuntimeRequest

sendInitError :: Request -> Manager -> String -> IO ()
sendInitError baseRuntimeRequest manager e =
  fmap (const ()) $ runtimeClientRetry $ flip httpNoBody manager $ toInitErrorRequest e baseRuntimeRequest


-- Helpers (mostly) for Headers

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


-- Helpers for Requests with JSON Bodies

httpValue :: Request -> Manager -> IO (Response Value)
httpValue request manager =
  withResponse request manager (\bodyReaderRes -> do
    value <- runConduit $ bodyReaderSource (responseBody bodyReaderRes) .| sinkParser value'
    return $ fmap (const value) bodyReaderRes
  )

bodyReaderSource :: MonadIO m
                 => BodyReader
                 -> ConduitM i BS.ByteString m ()
bodyReaderSource br =
    loop
  where
    loop = do
        bs <- liftIO $ brRead br
        unless (BS.null bs) $ do
            yield bs
            loop

-- Retry Helpers

runtimeClientRetryTry' :: Int -> Int -> IO (Response a) -> IO (Either HttpException (Response a))
runtimeClientRetryTry' retries maxRetries f
  | retries == maxRetries = try f
  | otherwise = do
    resOrEx <- try f
    let retry =
          threadDelay (500 * 2 ^ retries)
            >> runtimeClientRetryTry' (retries + 1) maxRetries f
    case resOrEx of
      Left (_ :: HttpException) -> retry
      Right res ->
        -- TODO: Explore this further.
        -- Before ~July 22nd 2020 it seemed that if a next event request reached
        -- the runtime before a new event was available that there would be a
        -- network error.  After it appears that a 403 is returned.
        if getResponseStatus res == status403 then retry
        else return $ Right res

runtimeClientRetryTry :: IO (Response a) -> IO (Either HttpException (Response a))
runtimeClientRetryTry = runtimeClientRetryTry' 0 10

runtimeClientRetry :: IO (Response a) -> IO (Response a)
runtimeClientRetry = fmap (either throw id) . runtimeClientRetryTry


-- Request Transformers

toNextEventRequest :: Request -> Request
toNextEventRequest = setRequestPath "2018-06-01/runtime/invocation/next"

toEventSuccessRequest :: ToJSON a => BS.ByteString -> a -> Request -> Request
toEventSuccessRequest reqId json =
  setRequestBodyJSON json .
  setRequestMethod "POST" .
  setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])

toBaseErrorRequest :: String -> Request -> Request
toBaseErrorRequest e =
  setRequestBodyLBS (encode (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"}))
    . setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
    . setRequestMethod "POST"
    . setRequestCheckStatus

toEventErrorRequest :: BS.ByteString -> String -> Request -> Request
toEventErrorRequest reqId e =
  setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"]) . toBaseErrorRequest e

toInitErrorRequest :: String -> Request -> Request
toInitErrorRequest e =
  setRequestPath "2018-06-01/runtime/init/error" . toBaseErrorRequest e


-- HTTP Client Type Helpers

getResponseBody :: Response a -> a
getResponseBody = responseBody

getResponseHeader :: HeaderName -> Response a -> [BS.ByteString]
getResponseHeader headerName = fmap snd . filter ((==) headerName . fst) . responseHeaders

getResponseStatus :: Response a -> Status
getResponseStatus = responseStatus

setRequestBodyJSON :: ToJSON a => a -> Request -> Request
setRequestBodyJSON = setRequestBodyLBS . encode

setRequestBodyLBS :: BSW.ByteString -> Request -> Request
setRequestBodyLBS body req = req { requestBody = RequestBodyLBS body }

setRequestHeader :: HeaderName -> [BS.ByteString] -> Request -> Request
setRequestHeader headerName values req =
  let
    withoutPrevious = filter ((/=) headerName . fst) $ requestHeaders req
    withNew = fmap ((,) headerName) values <> withoutPrevious
  in
    req { requestHeaders = withNew }

setRequestMethod :: BS.ByteString -> Request -> Request
setRequestMethod m req = req { method = m }

setRequestPath :: BS.ByteString -> Request -> Request
setRequestPath p req = req { path = p }
