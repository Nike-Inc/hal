{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : AWS.Lambda.Runtime.Value
Description : Runtime methods useful when constructing Haskell handlers for the AWS Lambda Custom Runtime.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable

These are runtimes designed for AWS Lambda, which accept a handler and return
an application that will retreive and execute events as long as a container
continues to exist.

These runtimes expect handlers that accept a parsed JSON AST
('Data.Aeson.Types.Value') as the input, instead some particular type with a FromJSON
instance.  Handlers using these runtimes must take care of the conversion and
handle errors explicitly.  Handlers that should throw an exception or never
expect to be invoked with an invalid payload, should simply use the runtimes in
the "AWS.Lambda.Runtime" module.

Each example shows the conversion from the Value type to the target FromJSON
type.

Many of these runtimes use "AWS.Lambda.Combinators" under the hood.
For those interested in peeking below the abstractions provided here,
please refer to that module.
-}

module AWS.Lambda.Runtime.Value (
  pureRuntime,
  pureRuntimeWithContext,
  fallibleRuntime,
  fallibleRuntimeWithContext,
  ioRuntime,
  ioRuntimeWithContext,
  readerTRuntime,
  mRuntimeWithContext',
  mRuntime,
  mRuntimeWithContext
) where

import           AWS.Lambda.RuntimeClient (RuntimeClientConfig, getRuntimeClientConfig,
                                           getNextData, sendEventError, sendEventSuccess)
import           AWS.Lambda.Combinators   (withoutContext)
import           AWS.Lambda.Context       (LambdaContext(..), HasLambdaContext(..))
import           Control.Exception        (SomeException, displayException)
import           Control.Monad            ((<=<), forever)
import           Control.Monad.Catch      (MonadCatch, try)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, local, runReaderT)
import           Data.Aeson               (ToJSON, Value)
import           Data.Bifunctor           (first)
import           Data.Text                (unpack)
import           System.Environment       (setEnv)

runtimeLoop :: (MonadCatch m, MonadIO m, ToJSON result) => RuntimeClientConfig -> (LambdaContext -> Value -> m result) -> m ()
runtimeLoop runtimeClientConfig fn = do
  -- Get an event
  (reqIdBS, event, eCtx) <- liftIO $ getNextData runtimeClientConfig

  -- Propagate the tracing header (Exception safe for this env var name)
  liftIO $ either (const (pure ())) (setEnv "_X_AMZN_TRACE_ID" . unpack . xRayTraceId) eCtx

  {- Catching like this is _usually_ considered bad practice, but this is a true
   case where we want to both catch all errors and propogate information about them.
   See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
  -}
  -- Put any exceptions in an Either
  caughtResult <- try (fn (either error id eCtx) event)
  -- Map the Either (via first) so it is an `Either String a`
  let result = first (displayException :: SomeException -> String) caughtResult

  liftIO $ case result of
    Right r -> sendEventSuccess runtimeClientConfig reqIdBS r
    Left e  -> sendEventError runtimeClientConfig reqIdBS e

-- | TODO
mRuntimeWithContext' :: (MonadCatch m, MonadIO m, ToJSON result) => (LambdaContext -> Value -> m result) -> m ()
mRuntimeWithContext' fn = do
  runtimeClientConfig <- liftIO getRuntimeClientConfig

  forever $ runtimeLoop runtimeClientConfig fn

-- | TODO
mRuntime :: (MonadCatch m, MonadIO m, ToJSON result) => (Value -> m result) -> m ()
mRuntime = mRuntimeWithContext' . withoutContext

-- | For any monad that supports IO\/catch\/Reader LambdaContext.
--
-- This function is problematic, and has been deprecated. The
-- 'HasLambdaContext' constraint requires that a 'LambdaContext' is
-- settable in the @m@ monad, but that is not the case - we only have
-- a 'LambdaContext' during the request/response cycle.
--
-- If you need caching behavours or are comfortable manipulating monad
-- transformers and want full control over your monadic interface,
-- consider 'mRuntimeWithContext''.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..), runReaderTLambdaContext)
-- import AWS.Lambda.Runtime (mRuntimeWithContext)
-- import Control.Monad.Reader (ReaderT, ask)
-- import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Value -> StateT Int (ReaderT LambdaContext IO) String
-- myHandler jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> return $ "My name is HAL, what's yours?"
--     Just Named { name } -> do
--       LambdaContext { functionName } <- ask
--       greeting <- liftIO $ getEnv \"GREETING\"
--
--       greetingCount <- get
--       put $ greetingCount + 1
--
--       return $ greeting ++ name ++ " (" ++ show greetingCount ++ ") from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = runReaderTLambdaContext (evalStateT (mRuntimeWithContext myHandler) 0)
-- @
{-# DEPRECATED mRuntimeWithContext "mRuntimeWithContext will be replaced by mRuntimeWithContext' in a future version. This type signature makes impossible promises - see the haddock for details." #-}
mRuntimeWithContext :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, ToJSON result) =>
  (Value -> m result) -> m ()
mRuntimeWithContext fn = mRuntimeWithContext' (\lc -> local (withContext lc) . fn)


-- | For functions that can read the lambda context and use IO within the same monad.
--
-- Use this for handlers that need any form of side-effect such as reading
-- environment variables or making network requests, and prefer to access the
-- AWS Lambda Context in the same monad.
-- However, do not use this runtime if you need stateful (caching) behaviors.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..))
-- import AWS.Lambda.Runtime (readerTRuntime)
-- import Control.Monad.Reader (ReaderT, ask)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Value -> ReaderT LambdaContext IO String
-- myHandler jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> return $ "My name is HAL, what's yours?"
--     Just Named { name } -> do
--       LambdaContext { functionName } <- ask
--       greeting <- liftIO $ getEnv \"GREETING\"
--       return $ greeting ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = readerTRuntime myHandler
-- @
readerTRuntime :: ToJSON result =>
  (Value -> ReaderT LambdaContext IO result) -> IO ()
readerTRuntime fn = mRuntimeWithContext' $ flip (runReaderT . fn)

-- | For functions with IO that can fail in a pure way (or via throw).
--
-- Use this for handlers that need any form of side-effect such as reading
-- environment variables or making network requests, and also need the
-- AWS Lambda Context as input.
-- However, do not use this runtime if you need stateful (caching) behaviors.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..))
-- import AWS.Lambda.Runtime (ioRuntimeWithContext)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Value -> IO (Either String String)
-- myHandler (LambdaContext { functionName }) jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> return $ pure "My name is HAL, what's yours?"
--     Just Named { name } -> do
--       greeting <- liftIO $ getEnv \"GREETING\"
--       return $ pure $ greeting ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = ioRuntimeWithContext myHandler
-- @
ioRuntimeWithContext :: ToJSON result =>
  (LambdaContext -> Value -> IO (Either String result)) -> IO ()
ioRuntimeWithContext fn = mRuntimeWithContext' (\lc -> either error pure <=< liftIO . fn lc)

-- | For functions with IO that can fail in a pure way (or via throw).
--
-- Use this for handlers that need any form of side-effect such as reading
-- environment variables or making network requests.
-- However, do not use this runtime if you need stateful (caching) behaviors.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Runtime (ioRuntime)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Value -> IO (Either String String)
-- myHandler jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> return $ pure "My name is HAL, what's yours?"
--     Just Named { name } -> do
--       greeting <- liftIO $ getEnv \"GREETING\"
--       return $ pure $ greeting ++ name
--
-- main :: IO ()
-- main = ioRuntime myHandler
-- @
ioRuntime :: ToJSON result =>
  (Value -> IO (Either String result)) -> IO ()
ioRuntime = ioRuntimeWithContext . withoutContext

-- | For pure functions that can still fail.
--
-- Use this for simple handlers that just translate input to output without side-effects,
-- but can fail and need the AWS Lambda Context as input.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..))
-- import AWS.Lambda.Runtime (fallibleRuntimeWithContext)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import Data.Text (unpack)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Value -> Either String String
-- myHandler (LambdaContext { functionName }) jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> Right "My name is HAL, what's yours?"
--     Just Named { name } ->
--       if name == \"World\" then
--         Right $ "Hello, World from " ++ unpack functionName ++ "!"
--       else
--         Left "Can only greet the world."
--
-- main :: IO ()
-- main = fallibleRuntimeWithContext myHandler
-- @
fallibleRuntimeWithContext :: ToJSON result =>
  (LambdaContext -> Value -> Either String result) -> IO ()
fallibleRuntimeWithContext = mRuntimeWithContext' . fmap (fmap pure)

-- | For pure functions that can still fail.
--
-- Use this for simple handlers that just translate input to output without side-effects,
-- but can fail.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Runtime (fallibleRuntime)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Value -> Either String String
-- myHandler jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> Right "My name is HAL, what's yours?"
--     Just Named { name } ->
--       if name == \"World\" then
--         Right "Hello, World!"
--       else
--         Left "Can only greet the world."
--
-- main :: IO ()
-- main = fallibleRuntime myHandler
-- @
fallibleRuntime :: ToJSON result =>
  (Value -> Either String result) -> IO ()
fallibleRuntime = fallibleRuntimeWithContext . withoutContext

-- | For pure functions that can never fail that also need access to the context.
--
-- Use this for simple handlers that just translate input to output without side-effects,
-- but that need the AWS Lambda Context as input.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..))
-- import AWS.Lambda.Runtime (pureRuntimeWithContext)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import Data.Text (unpack)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Value -> Either String String
-- myHandler (LambdaContext { functionName }) jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> Right "My name is HAL, what's yours?"
--     Just Named { name } ->
--       Right $ "Hello, " ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = pureRuntimeWithContext myHandler
-- @
pureRuntimeWithContext :: ToJSON result =>
  (LambdaContext -> Value -> result) -> IO ()
pureRuntimeWithContext = mRuntimeWithContext' . fmap (fmap pure)

-- | For pure functions that can never fail.
--
-- Use this for simple handlers that just translate input to output without side-effects.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
-- module Main where
--
-- import AWS.Lambda.Runtime (pureRuntime)
-- import Data.Aeson (Value, FromJSON, parseJSON)
-- import Data.Aeson.Types (parseMaybe)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Value -> String
-- myHandler jsonAst =
--   case parseMaybe parseJSON jsonAst of
--     Nothing -> "My name is HAL, what's yours?"
--     Just Named { name } ->
--       "Hello, " ++ name ++ "!"
--
-- main :: IO ()
-- main = pureRuntime myHandler
-- @
pureRuntime :: ToJSON result => (Value -> result) -> IO ()
pureRuntime = pureRuntimeWithContext . withoutContext
