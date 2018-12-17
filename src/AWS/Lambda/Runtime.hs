{-# LANGUAGE FlexibleContexts #-}
module AWS.Lambda.Runtime (
  pureLambdaRuntime,
  pureLambdaRuntimeWithContext,
  simpleLambdaRuntime,
  simpleLambdaRuntimeWithContext,
  ioLambdaRuntime,
  ioLambdaRuntimeWithContext,
  readerTLambdaRuntime,
  mLambdaContextRuntime,
  runReaderTLambdaContext,
  LambdaContext(..),
  HasLambdaContext(..),
  defConfig
) where

import           AWS.Lambda.RuntimeClient (getBaseRuntimeRequest, getNextEvent,
                                           sendEventError, sendEventSuccess, sendInitError)
import           Control.Exception        (SomeException, displayException)
import           Control.Monad            (forever)
import           Control.Monad.Catch      (MonadCatch, try)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, local,
                                           runReaderT)
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
  { getRemainingTimeInMillis :: Double, -- TODO this is calculated by "us", Nathan and I talked about moving this into a function.
    functionName             :: String,
    functionVersion          :: String,
    functionMemorySize       :: String,
    awsRequestId             :: String,
    logGroupName             :: String,
    logStreamName            :: String,
    -- The following context values come from headers rather than env vars.
    invokedFunctionArn       :: String,
    xRayTraceId              :: String,
    deadlineMs               :: Double
  } deriving (Show, Generic)

class HasLambdaContext r where
  withContext :: (LambdaContext -> r -> r)

instance HasLambdaContext LambdaContext where
  withContext _ c = c

instance DefConfig LambdaContext where
  defConfig = LambdaContext 0 "" "" "" "" "" "" "" "" 0

instance FromEnv LambdaContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

runtimeLoop :: (HasLambdaContext r, MonadReader r m, MonadCatch m, MonadIO m, FromJSON event, ToJSON result) => Request -> LambdaContext ->
  (event -> m result) -> m ()
runtimeLoop baseRuntimeRequest baseContext fn = do
  -- Get an event
  nextRes <- liftIO $ getNextEvent baseRuntimeRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  liftIO $ setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes
  let functionArn = head $ getResponseHeader "Lambda-Runtime-Invoked-Function-Arn" nextRes
  let deadlineInMs = head $ getResponseHeader "Lambda-Runtime-Deadline-Ms" nextRes

  -- Populate the context with values from headers
  let ctx = baseContext { awsRequestId       = BSC.unpack reqId,
                          xRayTraceId        = BSC.unpack traceId,
                          invokedFunctionArn = BSC.unpack functionArn,
                          -- TODO I think there's a cleaner/safer way to do this, but here it is for now.
                          deadlineMs         = read . BSC.unpack $ deadlineInMs
                        }

  local (withContext ctx) $ do

    result <- case getResponseBody nextRes of
      -- If we failed to parse or convert the JSON to the handler's event type, we consider
      -- it a handler error without ever calling it.
      Left ex -> return $ Left $ displayException ex

      -- Otherwise, we'll pass the event into the handler
      Right event -> do
        {- Catching like this is _usually_ considered bad practice, but this is a true
             case where we want to both catch all errors and propogate information about them.
             See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
        -}
        -- Put any exceptions in an Either
        caughtResult <- try (fn event)
        -- Map the Either (via first) so it is an `Either String a`
        return $ first (displayException :: SomeException -> String) caughtResult

    liftIO $ case result of
      Right r -> sendEventSuccess baseRuntimeRequest reqId r
      Left e  -> sendEventError baseRuntimeRequest reqId e

--TODO: Revisit all names before we put them under contract
-- | For any monad that supports IO/catch/Reader LambdaContext
mLambdaContextRuntime :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON event, ToJSON result) =>
  (event -> m result) -> m ()
mLambdaContextRuntime fn = do
  -- TODO: instead of returning a `baseRequest`, in the vein of hiding
  -- HTTP Details, we also could hide context retrieval details.
  -- So this method could simply retrieve the `runtimeClientConfig`,
  -- complete with error handling and baseContext/Request procurement
  -- The user of the RuntimeClient would not know what's in the config,
  -- they just are expected to pass it along.
  baseRuntimeRequest <- liftIO getBaseRuntimeRequest

  possibleCtx <- liftIO $ (decodeEnv :: IO (Either String LambdaContext))

  case possibleCtx of
    Left err -> liftIO $ sendInitError baseRuntimeRequest err
    Right baseContext -> forever $ runtimeLoop baseRuntimeRequest baseContext fn

-- | Helper for using arbitrary monads with only the LambdaContext in its Reader
runReaderTLambdaContext :: ReaderT LambdaContext m a -> m a
runReaderTLambdaContext = flip runReaderT defConfig

-- | For functions that can read the lambda context and use IO within the same monad.
readerTLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> ReaderT LambdaContext IO result) -> IO ()
readerTLambdaRuntime = runReaderTLambdaContext .  mLambdaContextRuntime

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
ioLambdaRuntimeWithContext fn = readerTLambdaRuntime (\event -> do
  config <- ask
  result <- liftIO $ fn config event
  case result of
    Left e  -> error e
    Right x -> return x
 )

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = ioLambdaRuntimeWithContext wrapped
    where wrapped _ e = fn e

-- | For pure functions that can still fail.
pureLambdaRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> Either String result) -> IO ()
pureLambdaRuntimeWithContext fn = ioLambdaRuntimeWithContext wrapped
  where wrapped c e = return $ fn c e

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
