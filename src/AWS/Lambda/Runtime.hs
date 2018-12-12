module AWS.Lambda.Runtime (
  pureLambdaRuntime,
  pureLambdaRuntimeWithContext,
  simpleLambdaRuntime,
  simpleLambdaRuntimeWithContext,
  ioLambdaRuntime,
  ioLambdaRuntimeWithContext,
  LambdaContext(..)
) where

import           AWS.Lambda.RuntimeClient (getBaseRuntimeRequest, getNextEvent,
                                           sendEventError, sendEventSuccess)
import           Control.Exception        (SomeException, displayException,
                                           evaluate, try)
import           Control.Monad            (forever, join)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Bifunctor           (first)
import qualified Data.ByteString.Char8    as BSC
import           GHC.Generics             (Generic)
import           Network.HTTP.Simple      (Request, getResponseBody,
                                           getResponseHeader)
import           System.Environment       (setEnv)
import           System.Envy              (DefConfig (..), FromEnv, Option (..),
                                           decodeEnv, fromEnv, gFromEnvCustom)

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

runtimeLoop :: (FromJSON event, ToJSON result) => Request ->
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
runtimeLoop baseRuntimeRequest fn = do
  -- Get an event
  nextRes <- getNextEvent baseRuntimeRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  possibleCtx <- (decodeEnv :: IO (Either String LambdaContext))

  case possibleCtx of
    Left err -> sendEventError baseRuntimeRequest reqId err
    Right ctx -> do
      result <- case getResponseBody nextRes of
        -- If we failed to parse or convert the JSON to the handler's event type, we consider
        -- it a handler error without ever calling it.
        Left ex -> evaluate $ Left $ displayException ex

        -- Otherwise, we'll pass the event into the handler
        Right event -> do
          {- Note1: catching like this is _usually_ considered bad practice, but this is a true
               case where we want to both catch all errors and propogate information about them.
               See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
             Note2: This might make BYOMonad harder, we're really exepecting an Either now.
               catch is the alternative, but has us working harder with the exception itself.
          -}
          -- Put the exception in an Either, so we get nested Eithers
          caughtResult <- try (fn ctx event)
          -- Map the outer Either (via first) so they are both of `Either String a`, then collapse them (via join)
          return $ join $ first (displayException :: SomeException -> String) caughtResult

      case result of
        Right r -> sendEventSuccess baseRuntimeRequest reqId r
        Left e  -> sendEventError baseRuntimeRequest reqId e

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
ioLambdaRuntimeWithContext fn = do
  baseRuntimeRequest <- getBaseRuntimeRequest
  forever $ runtimeLoop baseRuntimeRequest fn

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = ioLambdaRuntimeWithContext wrapped
    where wrapped _ e = fn e

-- | For pure functions that can still fail.
pureLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> Either String result) -> IO ()
pureLambdaRuntimeWithContext fn = ioLambdaRuntimeWithContext wrapped
  where wrapped c e = evaluate $ fn c e

-- | For pure functions that can still fail.
pureLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
pureLambdaRuntime fn = pureLambdaRuntimeWithContext wrapped
  where
    wrapped _ e = fn e

-- | For pure functions that can never fail.
simpleLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> result) -> IO ()
simpleLambdaRuntimeWithContext fn = pureLambdaRuntimeWithContext wrapped
  where wrapped c e = Right $ fn c e

-- | For pure functions that can never fail.
simpleLambdaRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
simpleLambdaRuntime fn = pureLambdaRuntime (Right . fn)
