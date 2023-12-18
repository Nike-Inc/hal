{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : AWS.Lambda.Runtime
Description : Runtime methods useful when constructing Haskell handlers for the AWS Lambda Custom Runtime.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable

These are runtimes designed for AWS Lambda, which accept a handler and return
an application that will retreive and execute events as long as a container
continues to exist.

Many of these runtimes use "AWS.Lambda.Combinators" under the hood.
For those interested in peeking below the abstractions provided here,
please refer to that module.
-}

module AWS.Lambda.Runtime (
  pureRuntime,
  pureRuntimeWithContext,
  fallibleRuntime,
  fallibleRuntimeWithContext,
  ioRuntime,
  ioRuntimeWithContext,
  readerTRuntime,
  mRuntime,
  mRuntimeWithContext
) where

import           AWS.Lambda.Combinators   (withInfallibleParse,
                                           withFallibleParse,
                                           withoutContext)
import           AWS.Lambda.Context       (LambdaContext(..))
import qualified AWS.Lambda.Runtime.Value as ValueRuntime
import           Control.Monad            (join)
import           Control.Monad.Catch      (MonadCatch)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (ReaderT)
import           Data.Aeson               (FromJSON, ToJSON)

-- | For any monad that supports 'IO' and 'catch'. Useful if you need
-- caching behaviours or are comfortable manipulating monad
-- transformers, and want full control over your monadic interface.
--
-- @
-- {-\# LANGUAGE DeriveGeneric, NamedFieldPuns \#-}
--
-- module Main where
--
-- import AWS.Lambda.Context (LambdaContext(..))
-- import AWS.Lambda.Runtime (mRuntimeWithContext)
-- import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (FromJSON)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Named -> StateT Int IO String
-- myHandler LambdaContext { functionName } Named { name } = do
--   greeting <- liftIO $ getEnv \"GREETING\"
--
--   greetingCount <- get
--   put $ greetingCount + 1
--
--   return $ greeting ++ name ++ " (" ++ show greetingCount ++ ") from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = evalStateT (mRuntimeWithContext myHandler) 0
-- @
mRuntimeWithContext :: (MonadCatch m, MonadIO m, FromJSON event, ToJSON result) => (LambdaContext -> event -> m result) -> m ()
mRuntimeWithContext = ValueRuntime.mRuntimeWithContext . fmap withInfallibleParse

-- | For any monad that supports 'IO' and 'catch'. Useful if you need
-- caching behaviours or are comfortable manipulating monad
-- transformers, want full control over your monadic interface, but
-- don't need to inspect the 'LambdaContext'.
--
-- @
-- {-\# LANGUAGE DeriveGeneric, NamedFieldPuns \#-}
--
-- module Main where
--
-- import AWS.Lambda.Runtime (mRuntime)
-- import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
-- import Control.Monad.Trans (liftIO)
-- import Data.Aeson (FromJSON)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Named -> StateT Int IO String
-- myHandler Named { name } = do
--   greeting <- liftIO $ getEnv \"GREETING\"
--
--   greetingCount <- get
--   put $ greetingCount + 1
--
--   return $ greeting ++ name ++ " (" ++ show greetingCount ++ ")!"
--
-- main :: IO ()
-- main = evalStateT (mRuntime myHandler) 0
-- @
mRuntime :: (MonadCatch m, MonadIO m, FromJSON event, ToJSON result) => (event -> m result) -> m ()
mRuntime = ValueRuntime.mRuntime . withInfallibleParse

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
-- import Data.Aeson (FromJSON)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Named -> ReaderT LambdaContext IO String
-- myHandler Named { name } = do
--   LambdaContext { functionName } <- ask
--   greeting <- liftIO $ getEnv \"GREETING\"
--   return $ greeting ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = readerTRuntime myHandler
-- @
readerTRuntime :: (FromJSON event, ToJSON result) =>
  (event -> ReaderT LambdaContext IO result) -> IO ()
readerTRuntime = ValueRuntime.readerTRuntime . withInfallibleParse

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
-- import Data.Aeson (FromJSON)
-- import Data.Text (unpack)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Named -> IO (Either String String)
-- myHandler (LambdaContext { functionName }) (Named { name }) = do
--   greeting <- getEnv \"GREETING\"
--   return $ pure $ greeting ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = ioRuntimeWithContext myHandler
-- @
ioRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> IO (Either String result)) -> IO ()
ioRuntimeWithContext = ValueRuntime.ioRuntimeWithContext . fmap withInfallibleParse

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
-- import Data.Aeson (FromJSON)
-- import System.Environment (getEnv)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Named -> IO (Either String String)
-- myHandler (Named { name }) = do
--   greeting <- getEnv \"GREETING\"
--   return $ pure $ greeting ++ name
--
-- main :: IO ()
-- main = ioRuntime myHandler
-- @
ioRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioRuntime = ValueRuntime.ioRuntime . withInfallibleParse

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
-- import Data.Aeson (FromJSON)
-- import Data.Text (unpack)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Named -> Either String String
-- myHandler (LambdaContext { functionName }) (Named { name }) =
--   if name == \"World\" then
--     Right $ "Hello, World from " ++ unpack functionName ++ "!"
--   else
--     Left "Can only greet the world."
--
-- main :: IO ()
-- main = fallibleRuntimeWithContext myHandler
-- @
fallibleRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> Either String result) -> IO ()
fallibleRuntimeWithContext fn = ValueRuntime.fallibleRuntimeWithContext $ \lc -> join . withFallibleParse (fn lc)

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
-- import Data.Aeson (FromJSON)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Named -> Either String String
-- myHandler (Named { name }) =
--   if name == \"World\" then
--     Right "Hello, World!"
--   else
--     Left "Can only greet the world."
--
-- main :: IO ()
-- main = fallibleRuntime myHandler
-- @
fallibleRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
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
-- import Data.Aeson (FromJSON)
-- import Data.Text (unpack)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: LambdaContext -> Named -> String
-- myHandler (LambdaContext { functionName }) (Named { name }) =
--   "Hello, " ++ name ++ " from " ++ unpack functionName ++ "!"
--
-- main :: IO ()
-- main = pureRuntimeWithContext myHandler
-- @
pureRuntimeWithContext :: (FromJSON event, ToJSON result) =>
  (LambdaContext -> event -> result) -> IO ()
pureRuntimeWithContext = fallibleRuntimeWithContext . fmap (fmap pure)

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
-- import Data.Aeson (FromJSON)
-- import GHC.Generics (Generic)
--
-- data Named = Named {
--   name :: String
-- } deriving Generic
-- instance FromJSON Named
--
-- myHandler :: Named -> String
-- myHandler Named { name } = "Hello, " ++ name ++ "!"
--
-- main :: IO ()
-- main = pureRuntime myHandler
-- @
pureRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
pureRuntime = pureRuntimeWithContext . withoutContext
