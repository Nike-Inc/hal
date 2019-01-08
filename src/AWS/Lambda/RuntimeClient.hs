module AWS.Lambda.RuntimeClient (
  getBaseRuntimeRequest,
  getNextEvent,
  getNextEvent',
  postEventSuccess,
  postEventSuccess',
  postEventError,
  postInitError
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

-- Damn this thing is uuuuuugly
getNextEvent' :: FromJSON a => Request -> IO (Response (Either JSONException a))
getNextEvent' baseRuntimeRequest = do
  resOrEx <- try $ getNextEvent baseRuntimeRequest
  let checkStatus res = if not $ statusIsSuccessful $ getResponseStatus res then
        Left "Unexpected Runtime Error:  Could not retrieve next event."
      else
        Right res
  let resOrMsg = first (displayException :: HttpException -> String) resOrEx >>= checkStatus
  case resOrMsg of
    Left msg -> do
      _ <- postInitError baseRuntimeRequest msg
      error msg
    Right y -> return y

-- Damn this thing is uuuuuugly
postEventSuccess' :: ToJSON a => Request -> BS.ByteString -> a -> IO ()
postEventSuccess' baseRuntimeRequest reqId json = do
  resOrEx <- try $ postEventSuccess baseRuntimeRequest reqId json

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
      postEventError baseRuntimeRequest reqId msg
    Left (Right msg) -> error msg
    Right () -> return ()

getBaseRuntimeRequest :: IO Request
getBaseRuntimeRequest = do
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  parseRequest $ "http://" ++ awsLambdaRuntimeApi

-- TODO: Would a "genHandlers" method make sense that returns all runtime handlers
-- with the baseRuntimeRequest pre-injected?
-- Then these functions serve as a very low-level API
getNextEvent :: FromJSON a => Request -> IO (Response (Either JSONException a))
getNextEvent = httpJSONEither . setRequestPath "2018-06-01/runtime/invocation/next"

postEventSuccess :: ToJSON a => Request -> BS.ByteString -> a -> IO (Response ())
postEventSuccess baseRuntimeRequest reqId json = do
  let successUrl
        = setRequestBodyJSON json
        $ setRequestMethod "POST"
        $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])
        $ baseRuntimeRequest
  httpNoBody successUrl

toBaseErrorRequest :: String -> Request -> Request
toBaseErrorRequest e baseRuntimeRequest =
  -- TODO: setRequestBodyJSON is overriding our "Content-Type" header
  setRequestBodyJSON (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"})
    $ setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
    $ setRequestMethod "POST"
    $ setRequestCheckStatus
    $ baseRuntimeRequest

postEventError :: Request -> BS.ByteString -> String -> IO ()
postEventError baseRuntimeRequest reqId e = do
  let failureUrl
        = setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"])
        $ toBaseErrorRequest e
        $ baseRuntimeRequest
  _ <- httpNoBody failureUrl
  return ()

postInitError :: Request -> String -> IO ()
postInitError baseRuntimeRequest e = do
  let failureUrl
        = setRequestPath "2018-06-01/runtime/init/error"
        $ toBaseErrorRequest e
        $ baseRuntimeRequest
  _ <- httpNoBody failureUrl
  return ()
