module AWS.Lambda.Runtime (
  ioLambdaRuntime,
  pureLambdaRuntime,
  pureLambdaRuntimeWithContext,
  simpleLambdaRuntime,
  simpleLambdaRuntimeWithContext,
  LambdaContext(..)
) where

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

-- TODO not sure how I feel about defaults yet, I think we may want to report
-- some kind of error if we don't find it in the environment?
instance DefConfig LambdaContext where
  defConfig = LambdaContext 0 "" "" "" "" "" "" "" 0

instance FromEnv LambdaContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = forever $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  baseRequest <- parseRequest $ "http://" ++ awsLambdaRuntimeApi

  -- Get an event
  nextRes <- httpJSON $ setRequestPath "2018-06-01/runtime/invocation/next" baseRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  possibleCtx <- (decodeEnv :: IO (Either String LambdaContext))
  -- TODO this is a temporary hack before merging in Nathan's error handling work
  let ctx = head $ Data.Either.rights [possibleCtx]

  let event = getResponseBody nextRes
  {- Note1: catching like this is _usually_ considered bad practice, but this is a true
       case where we want to both catch all errors and propogate information about them.
       See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
     Note2: This might make BYOMonad harder, we're really exepecting an Either now.
       catch is the alternative, but has us working harder with the exception itself.
  -}
  -- Put the exception in an Either, so we get nested Eithers
  caughtResult <- try (fn ctx event)
  -- Map the outer Either (via first) so they are both of `Either String a`, then collapse them (via join)
  let result = join $ first (displayException :: SomeException -> String) caughtResult

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
