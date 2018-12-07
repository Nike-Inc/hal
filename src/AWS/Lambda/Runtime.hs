module AWS.Lambda.Runtime (
  pureLambdaRuntime,
  simpleLambdaRuntime
) where

import           Control.Monad         (forever)
import           Data.Aeson            (FromJSON (..), ToJSON (..))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           GHC.Generics          (Generic (..))
import           Network.HTTP.Simple   (getResponseBody, getResponseHeader,
                                        httpJSON, httpNoBody, parseRequest,
                                        setRequestBodyJSON, setRequestHeader,
                                        setRequestMethod, setRequestPath)
import           System.Environment    (getEnv, setEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

-- | For pure functions that can still fail.
pureLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
pureLambdaRuntime fn = forever $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  baseRequest <- parseRequest $ "http://" ++ awsLambdaRuntimeApi

  -- Get an event
  nextRes <- httpJSON $ setRequestPath "2018-06-01/runtime/invocation/next" baseRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  -- TODO: Create a context object
  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  let event = getResponseBody nextRes
  let result = fn event

  case result of
    Right r -> do
      -- Handle the response (successes)
      let successUrl
            = setRequestBodyJSON r
            $ setRequestMethod "POST"
            $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])
            $ baseRequest
      _ <- httpNoBody successUrl

      -- TODO: Handle errors
      return ()

    Left e -> do
      let failureUrl
            = setRequestBodyJSON (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"})
            $ setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
            $ setRequestMethod "POST"
            $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"])
            $ baseRequest
      _ <- httpNoBody failureUrl
      return ()

-- | For pure functions that can never fail.
simpleLambdaRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
simpleLambdaRuntime fn =  pureLambdaRuntime (Right . fn)
