module AWS.Lambda.Runtime (
  ioLambdaRuntime,
  pureLambdaRuntime,
  pureLambdaRuntimeWithContext,
  simpleLambdaRuntime,
  simpleLambdaRuntimeWithContext,
  LambdaContext(..)
) where

import           AWS.Lambda.RuntimeClient (getBaseRuntimeRequest, getNextEvent, sendEventSuccess,
                                           sendEventError)
import           Data.Bifunctor        (first)
import           Control.Exception     (displayException, evaluate, SomeException, try)
import           Control.Monad         (forever, join)
import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple   (getResponseBody, getResponseHeader,
                                        httpJSON, httpNoBody, parseRequest,
                                        setRequestBodyJSON, setRequestHeader,
                                        setRequestMethod, setRequestPath)
import           System.Environment    (getEnv, setEnv)
import System.Envy (FromEnv, DefConfig(..), decodeEnv, fromEnv, gFromEnvCustom, Option(..))
import Data.Either (rights)

data LambdaContext = LambdaContext
  { getRemainingTimeInMillis :: Double, -- TODO doesn't seem to be in env
    functionName             :: String,
    functionVersion          :: String,
    functionMemorySize       :: String,
    invokedFunctionArn       :: String, -- TODO doesn't seem to be in env
    awsRequestId             :: String, -- TODO doesn't seem to be in env
    logGroupName             :: String,
    logStreamName            :: String,
    deadlineMs               :: Double  -- TODO doesn't seem to be in env
  } deriving (Show, Generic)

instance DefConfig LambdaContext where
  defConfig = LambdaContext 0 "" "" "" "" "" "" "" 0

instance FromEnv LambdaContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = forever $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  baseRequest <- parseRequest $ "http://" ++ awsLambdaRuntimeApi

runtimeLoop :: (FromJSON event, ToJSON result) => Request ->
  (event -> IO (Either String result)) -> IO ()
runtimeLoop baseRuntimeRequest fn = do
  -- Get an event
  nextRes <- getNextEvent baseRuntimeRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  possibleCtx <- (decodeEnv :: IO (Either String LambdaContext))

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = do
  baseRuntimeRequest <- getBaseRuntimeRequest
  forever $ runtimeLoop baseRuntimeRequest fn

pureLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> Either String result) -> IO ()
pureLambdaRuntimeWithContext fn = ioLambdaRuntime wrapped
  where wrapped c e = evaluate $ fn c e

-- | For pure functions that can still fail.
pureLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
pureLambdaRuntime fn = pureLambdaRuntimeWithContext wrapped
  where
    wrapped _ e = fn e

simpleLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> result) -> IO ()
simpleLambdaRuntimeWithContext fn = pureLambdaRuntimeWithContext wrapped
  where wrapped c e = Right $ fn c e

-- | For pure functions that can never fail.
simpleLambdaRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
simpleLambdaRuntime fn = pureLambdaRuntime (Right . fn)
