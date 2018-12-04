module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 hiding (putStrLn)
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Text.Encoding
import GHC.Generics
import Network.HTTP.Req
import System.Environment

data HardCodedEvent = HardCodedEvent
  { value  :: Int
  } deriving (Show, Generic)

instance ToJSON HardCodedEvent
instance FromJSON HardCodedEvent

main :: IO ()
main = runReq def $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- liftIO $ getEnv "AWS_LAMBDA_RUNTIME_API"
  -- TODO: Is baseOptions an appropriate way to get/pass the port?
  let (baseUrl, baseOptions) = fromJust (parseUrlHttp (pack ("http://" ++ awsLambdaRuntimeApi)))

  -- Get an event
  let nextUrl = baseUrl /: "2018-06-01" /: "runtime" /: "invocation" /: "next"
  nextRes <- req GET nextUrl NoReqBody jsonResponse baseOptions

  -- Propagate the tracing header
  let traceId = fromJust $ responseHeader nextRes "Lambda-Runtime-Trace-Id"
  liftIO $ setEnv "_X_AMZN_TRACE_ID" (unpack traceId)

  -- TODO: Create a context object
  let reqId = fromJust $ responseHeader nextRes "Lambda-Runtime-Aws-Request-Id"

  -- TODO: Invoke the function handler
  let event = responseBody nextRes :: HardCodedEvent

  -- Handle the response (successes)
  let successUrl = baseUrl /: "runtime" /: "invocation" /: decodeUtf8 reqId /: "response"
  _ <- req POST successUrl (ReqBodyJson event) ignoreResponse baseOptions

  -- TODO: Handle errors
  return ()
