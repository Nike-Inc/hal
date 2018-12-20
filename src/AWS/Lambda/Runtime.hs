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
) where

import           AWS.Lambda.RuntimeClient (getBaseRuntimeRequest, getNextEvent,
                                           sendEventError, sendEventSuccess, sendInitError)
import           AWS.Lambda.Context       (LambdaContext(..), HasLambdaContext(..))
import           Control.Applicative      ((<*>), liftA2)
import           Control.Exception        (SomeException, displayException)
import           Control.Monad            (forever)
import           Control.Monad.Catch      (MonadCatch, try)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, local,
                                           runReaderT)
import           Data.Aeson               (FromJSON, ToJSON, decode)
import           Data.Bifunctor           (first)
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as BSW
import qualified Data.ByteString.Internal as BSI
import           Network.HTTP.Simple      (Request, getResponseBody,
                                           getResponseHeader)
import           System.Environment       (setEnv)
import           System.Envy              (decodeEnv, defConfig)

exactlyOneHeader :: [a] -> Maybe a
exactlyOneHeader [a] = Just a
exactlyOneHeader _ = Nothing

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b ma = case ma of
  Nothing -> Left b
  Just a -> Right a

-- Note: Does not allow whitespace
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x,"")] -> Just x
  _ -> Nothing

-- TODO: There must be a better way to do this
decodeHeaderValue :: FromJSON a => BSC.ByteString -> Maybe a
decodeHeaderValue = decode . BSW.pack . fmap BSI.c2w . BSC.unpack

-- An empty array means we successfully decoded, but nothing was there
-- If we have exactly one element, our outer maybe signals successful decode,
--   and our inner maybe signals that there was content sent
-- If we had more than one header value, the event was invalid
decodeOptionalHeader :: FromJSON a => [BSC.ByteString] -> Maybe (Maybe a)
decodeOptionalHeader header =
  case header of
    [] -> Just Nothing
    [x] -> fmap Just $ decodeHeaderValue x
    _ -> Nothing


runtimeLoop :: (HasLambdaContext r, MonadReader r m, MonadCatch m, MonadIO m, FromJSON event, ToJSON result) => Request -> LambdaContext ->
  (event -> m result) -> m ()
runtimeLoop baseRuntimeRequest baseContext fn = do
  -- Get an event
  nextRes <- liftIO $ getNextEvent baseRuntimeRequest

  -- If we got an event but our requestId is invalid/missing, there's no hope of meaningful recovery
  let reqIdBS = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  let mTraceId = fmap BSC.unpack $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  let mFunctionArn = fmap BSC.unpack $ exactlyOneHeader $ getResponseHeader "Lambda-Runtime-Invoked-Function-Arn" nextRes
  let mDeadlineMs = readMaybe . BSC.unpack =<< exactlyOneHeader (getResponseHeader "Lambda-Runtime-Deadline-Ms" nextRes)

  let mClientContext = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Client-Context" nextRes
  let mIdentity = decodeOptionalHeader $ getResponseHeader "Lambda-Runtime-Cognito-Identity" nextRes

  -- TODO: If this was DynamicContext, and we had a `mkContext :: StaticContext -> DynamicContext -> LambdaContext`
  -- this would be not be needed and would just have:
  -- fmap mkContext $ DynamicContext <$> mTraceId <*> mFunctionArn <*> mDeadlineMs <*> mClientContext <*> mIdentity
  let buildContext a b c d e =
        baseContext { awsRequestId       = BSC.unpack reqIdBS,
                      xRayTraceId        = a,
                      invokedFunctionArn = b,
                      deadlineMs         = c,
                      clientContext      = d,
                      identity           = e
                    }

  -- Populate the context with values from headers
  let mCtx = buildContext
               <$> mTraceId
               <*> mFunctionArn
               <*> mDeadlineMs
               <*> mClientContext
               <*> mIdentity
  let eCtx = maybeToEither "Runtime Error: Unable to decode Context from event response." mCtx

  let eEvent = first displayException $ getResponseBody nextRes

  result <- case liftA2 (,) eCtx eEvent of
    Left e -> return $ Left e
    Right (ctx, event) ->
      local (withContext ctx) $ do
        -- Propagate the tracing header (Exception safe for this env var name)
        liftIO $ setEnv "_X_AMZN_TRACE_ID" (xRayTraceId ctx)

        {- Catching like this is _usually_ considered bad practice, but this is a true
             case where we want to both catch all errors and propogate information about them.
             See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
        -}
        -- Put any exceptions in an Either
        caughtResult <- try (fn event)
        -- Map the Either (via first) so it is an `Either String a`
        return $ first (displayException :: SomeException -> String) caughtResult

  liftIO $ case result of
    Right r -> sendEventSuccess baseRuntimeRequest reqIdBS r
    Left e  -> sendEventError baseRuntimeRequest reqIdBS e

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
