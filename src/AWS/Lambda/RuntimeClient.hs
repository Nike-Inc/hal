module AWS.Lambda.RuntimeClient (
  getBaseRuntimeRequest,
  getNextEvent,
  postSuccess,
  postHandlerError
) where

import           Data.Aeson.Types      (FromJSON, ToJSON)
import qualified Data.ByteString       as BS
import           GHC.Generics          (Generic (..))
import           Network.HTTP.Simple   (Request, Response, parseRequest, JSONException,
                                        setRequestPath, httpJSONEither, setRequestBodyJSON,
                                        setRequestMethod, httpNoBody, setRequestHeader)
import           System.Environment    (getEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

getBaseRuntimeRequest :: IO Request
getBaseRuntimeRequest = do
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  parseRequest $ "http://" ++ awsLambdaRuntimeApi

-- TODO: Would a "genHandlers" method make sense that returns all runtime handlers
-- with the baseRuntimeRequest pre-injected?
-- Then these functions serve as a very low-level API
getNextEvent :: FromJSON a => Request -> IO (Response (Either JSONException a))
getNextEvent = httpJSONEither . setRequestPath "2018-06-01/runtime/invocation/next"

postSuccess :: ToJSON a => Request -> BS.ByteString -> a -> IO ()
postSuccess baseRuntimeRequest reqId json = do
  let successUrl
        = setRequestBodyJSON json
        $ setRequestMethod "POST"
        $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])
        $ baseRuntimeRequest
  _ <- httpNoBody successUrl
  return ()


postHandlerError :: Request -> BS.ByteString -> String -> IO ()
postHandlerError baseRuntimeRequest reqId e = do
  let failureUrl
        = setRequestBodyJSON (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"})
        $ setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
        $ setRequestMethod "POST"
        $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"])
        $ baseRuntimeRequest
  _ <- httpNoBody failureUrl
  return ()
