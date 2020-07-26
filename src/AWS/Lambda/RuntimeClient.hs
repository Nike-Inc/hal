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
  getNextEvent,
  sendEventSuccess,
  sendEventError,
  sendInitError
) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (displayException, try, throw)
import           Data.Aeson                (encode)
import           Data.Aeson.Types          (FromJSON, ToJSON)
import           Data.Bifunctor            (first)
import qualified Data.ByteString           as BS
import           GHC.Generics              (Generic (..))
import           Network.HTTP.Simple       (HttpException, JSONException,
                                            Request, Response,
                                            getResponseStatus, httpJSONEither,
                                            httpNoBody, parseRequest,
                                            setRequestBodyJSON,
                                            setRequestBodyLBS,
                                            setRequestCheckStatus,
                                            setRequestHeader, setRequestMethod,
                                            setRequestPath)
import           Network.HTTP.Types.Status (status403, status413, statusIsSuccessful)
import           System.Environment        (getEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

data RuntimeClientConfig = RuntimeClientConfig Request

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
  return $ RuntimeClientConfig req

getNextEvent :: FromJSON a => RuntimeClientConfig -> IO (Response (Either JSONException a))
getNextEvent rcc@(RuntimeClientConfig baseRuntimeRequest) = do
  resOrEx <- runtimeClientRetryTry $ httpJSONEither $ toNextEventRequest baseRuntimeRequest
  let checkStatus res = if not $ statusIsSuccessful $ getResponseStatus res then
        Left "Unexpected Runtime Error:  Could not retrieve next event."
      else
        Right res
  let resOrMsg = first (displayException :: HttpException -> String) resOrEx >>= checkStatus
  case resOrMsg of
    Left msg -> do
      _ <- sendInitError rcc msg
      error msg
    Right y -> return y

sendEventSuccess :: ToJSON a => RuntimeClientConfig -> BS.ByteString -> a -> IO ()
sendEventSuccess rcc@(RuntimeClientConfig baseRuntimeRequest) reqId json = do
  resOrEx <- runtimeClientRetryTry $ httpNoBody $ toEventSuccessRequest reqId json baseRuntimeRequest

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
sendEventError (RuntimeClientConfig baseRuntimeRequest) reqId e =
  fmap (const ()) $ runtimeClientRetry $ httpNoBody $ toEventErrorRequest reqId e baseRuntimeRequest

sendInitError :: RuntimeClientConfig -> String -> IO ()
sendInitError (RuntimeClientConfig baseRuntimeRequest) e =
  fmap (const ()) $ runtimeClientRetry $ httpNoBody $ toInitErrorRequest e baseRuntimeRequest

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
