module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString hiding (head, unpack)
import Data.ByteString.Char8 hiding (head)
import GHC.Generics
import Network.HTTP.Simple
import System.Environment

data HardCodedEvent = HardCodedEvent
  { value  :: Int
  } deriving (Show, Generic)

instance ToJSON HardCodedEvent
instance FromJSON HardCodedEvent

main :: IO ()
main = forever $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- liftIO $ getEnv "AWS_LAMBDA_RUNTIME_API"
  -- TODO: Is baseOptions an appropriate way to get/pass the port?
  baseRequest <- parseRequest $ "http://" ++ awsLambdaRuntimeApi

  -- Get an event
  nextRes <- httpJSON $ setRequestPath "2018-06-01/runtime/invocation/next" baseRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  liftIO $ setEnv "_X_AMZN_TRACE_ID" (unpack traceId)

  -- TODO: Create a context object
  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  -- TODO: Invoke the function handler
  let event = getResponseBody nextRes :: HardCodedEvent

  -- Handle the response (successes)
  let successUrl
        = setRequestBodyJSON event
        $ setRequestMethod "POST"
        $ setRequestPath (Data.ByteString.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])
        $ baseRequest
  r <- httpNoBody successUrl

  -- TODO: Handle errors
  return ()
