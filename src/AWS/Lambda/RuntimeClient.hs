module AWS.Lambda.RuntimeClient (
  getBaseRuntimeRequest,
  getNextEvent,
  sendEventSuccess,
  sendEventError
) where

import           Control.Exception         (displayException, try)
import           Data.Aeson.Types          (FromJSON, ToJSON)
import           Data.Bifunctor            (first)
import qualified Data.ByteString           as BS
import           GHC.Generics              (Generic (..))
import           Network.HTTP.Simple       (Request, Response, parseRequest, JSONException,
                                            setRequestPath, httpJSONEither, setRequestBodyJSON,
                                            setRequestMethod, httpNoBody, setRequestHeader,
                                            HttpException, getResponseStatus, setRequestCheckStatus)
import           Network.HTTP.Types.Status (statusIsSuccessful, status413)
import           System.Environment        (getEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

-- Exposed Handlers

-- TODO: Would a "genHandlers" method make sense that returns all runtime handlers
-- with the baseRuntimeRequest pre-injected?
-- Then these functions serve as a very low-level API
getBaseRuntimeRequest :: IO Request
getBaseRuntimeRequest = do
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  parseRequest $ "http://" ++ awsLambdaRuntimeApi

getNextEvent :: FromJSON a => Request -> IO (Response (Either JSONException a))
getNextEvent baseRuntimeRequest = do
  resOrEx <- try $ httpJSONEither $ toNextEventRequest baseRuntimeRequest
  let checkStatus res = if not $ statusIsSuccessful $ getResponseStatus res then
        Left "Unexpected Runtime Error:  Could retrieve next event."
      else
        Right res
  let resOrMsg = first (displayException :: HttpException -> String) resOrEx >>= checkStatus
  case resOrMsg of
    Left msg -> do
      _ <- httpNoBody $ toInitErrorRequest msg baseRuntimeRequest
      error msg
    Right y -> return y

sendEventSuccess :: ToJSON a => Request -> BS.ByteString -> a -> IO ()
sendEventSuccess baseRuntimeRequest reqId json = do
  resOrEx <- try $ httpNoBody $ toEventSuccessRequest reqId json baseRuntimeRequest

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
  fmap (const ()) $ httpNoBody $ toEventErrorRequest reqId e baseRuntimeRequest


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
  -- TODO: setRequestBodyJSON is overriding our "Content-Type" header
  setRequestBodyJSON (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"})
    . setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
    . setRequestMethod "POST"
    . setRequestCheckStatus

toEventErrorRequest :: BS.ByteString -> String -> Request -> Request
toEventErrorRequest reqId e =
  setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"]) . toBaseErrorRequest e

toInitErrorRequest :: String -> Request -> Request
toInitErrorRequest e =
  setRequestPath "2018-06-01/runtime/init/error" . toBaseErrorRequest e
