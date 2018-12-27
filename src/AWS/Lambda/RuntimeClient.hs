{-|
Module      : AWS.Lambda.RuntimeClient
Description : HTTP related machinery for talking to the AWS Lambda Custom Runtime interface.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.RuntimeClient (
  getBaseRuntimeRequest,
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
import           Network.HTTP.Types.Status (status413, statusIsSuccessful)
import           System.Environment        (getEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

-- Exposed Handlers

-- TODO: It would be interesting if we could make the interface a sort of
-- "chained" callback API.  So instead of getting back a base request to kick
-- things off we get a 'getNextEvent' handler and then the 'getNextEvent'
-- handler returns both the 'success' and 'error' handlers.  So things like
-- baseRequest and reqId are pre-injected.
getBaseRuntimeRequest :: IO Request
getBaseRuntimeRequest = do
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  parseRequest $ "http://" ++ awsLambdaRuntimeApi

getNextEvent :: FromJSON a => Request -> IO (Response (Either JSONException a))
getNextEvent baseRuntimeRequest = do
  resOrEx <- runtimeClientRetryTry $ httpJSONEither $ toNextEventRequest baseRuntimeRequest
  let checkStatus res = if not $ statusIsSuccessful $ getResponseStatus res then
        Left "Unexpected Runtime Error:  Could retrieve next event."
      else
        Right res
  let resOrMsg = first (displayException :: HttpException -> String) resOrEx >>= checkStatus
  case resOrMsg of
    Left msg -> do
      _ <- sendInitError baseRuntimeRequest msg
      error msg
    Right y -> return y

sendEventSuccess :: ToJSON a => Request -> BS.ByteString -> a -> IO ()
sendEventSuccess baseRuntimeRequest reqId json = do
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
      sendEventError baseRuntimeRequest reqId msg
    Left (Right msg) -> error msg
    Right () -> return ()

sendEventError :: Request -> BS.ByteString -> String -> IO ()
sendEventError baseRuntimeRequest reqId e =
  fmap (const ()) $ runtimeClientRetry $ httpNoBody $ toEventErrorRequest reqId e baseRuntimeRequest

sendInitError :: Request -> String -> IO ()
sendInitError baseRuntimeRequest e =
  fmap (const ()) $ runtimeClientRetry $ httpNoBody $ toInitErrorRequest e baseRuntimeRequest

-- Retry Helpers

runtimeClientRetryTry' :: Int -> IO (Response a) -> IO (Either HttpException (Response a))
runtimeClientRetryTry' 1 f = try f
runtimeClientRetryTry' i f = do
  resOrEx <- try f
  case resOrEx of
    Left (_ :: HttpException) -> threadDelay 500 >> runtimeClientRetryTry' (i - 1) f
    Right res -> return $ Right res

runtimeClientRetryTry :: IO (Response a) -> IO (Either HttpException (Response a))
runtimeClientRetryTry = runtimeClientRetryTry' 3

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
