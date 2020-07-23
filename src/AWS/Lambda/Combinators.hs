{-|
Module      : AWS.Lambda.Combinators
Description : Function transformers that can be used to adapt the base runtime into other useful interfaces.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable

These combinators are for those who need to peek below the abstraction of the basic runtimes, for whatever reason.

They map functions (instead of values) to turn basic handlers into handlers compatible with the base runtime.  These combinators allow us to expose functionality across many dimensions in an abstract way.  It also allows simple building blocks for those who need to "get in the middle" or adapt the basic runtimes in new ways without rebuilding everything from the ground up.
-}

module AWS.Lambda.Combinators (
    withIOInterface,
    withFallibleInterface,
    withPureInterface,
    withoutContext,
    withInfallibleParse
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson             (FromJSON, parseJSON, Value)
import           Data.Aeson.Types       (parseEither)


-- Helper for converting an either result into a monad/exception
dropEither :: Monad m => Either String a -> m a
dropEither = \case 
     Left e  -> error e
     Right x -> return x


-- | Upgrades a handler that uses the `IO` monad with an `Either` inside into a
-- base runtime handler.
--
-- In the example below, we reconstruct 'AWS.Lambda.Runtime.ioRuntimeWithContext'
-- without actually using it. The 'AWS.Lambda.Runtime.readerTRuntime' expects
-- a handler in the form of @event -> ReaderT LambdaContext IO result@
-- (ignoring constraints).  By composing it with `withIOInterface` we get a new runtime which
-- expects a function in the form of @LambdaContext -> event -> IO result@
-- which matches that of `myHandler`.
--
-- @
--     {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
--     module Main where
--
--     import AWS.Lambda.Context (LambdaContext(..))
--     import AWS.Lambda.Runtime (readerTRuntime)
--     import AWS.Lambda.Combinators (withIOInterface)
--     import Data.Aeson (FromJSON)
--     import Data.Text (unpack)
--     import System.Environment (getEnv)
--     import GHC.Generics (Generic)
--
--     data Named = Named {
--       name :: String
--     } deriving Generic
--     instance FromJSON Named
--
--     myHandler :: LambdaContext -> Named -> IO (Either String String)
--     myHandler (LambdaContext { functionName }) (Named { name }) = do
--       greeting <- getEnv \"GREETING\"
--       return $ if name == \"World\" then
--         Right $ "Hello, World from " ++ unpack functionName ++ "!"
--       else
--         Left "Can only greet the world."
--
--     main :: IO ()
--     main = (readerTRuntime . withIOInterface) myHandler
-- @
withIOInterface :: (MonadReader c m, MonadIO m) => (c -> b -> IO (Either String a)) -> (b -> m a)
withIOInterface fn event = do
  config <- ask
  result <- liftIO $ fn config event
  dropEither result

-- | Upgrades a handler that accepts 'AWS.Lambda.Context.LambdaContext' and
-- an event to return a value inside an `Either` inside into a base runtime handler.
--
-- In the example below, we reconstruct 'AWS.Lambda.Runtime.fallibleRuntimeWithContext'
-- without actually using it.  The 'AWS.Lambda.Runtime.readerTRuntime' expects a handler
-- in the form of @event -> ReaderT LambdaContext IO result@ (ignoring constraints).
-- By composing it with `withFallibleInterface` we get a new runtime which
-- expects a function in the form of @LambdaContext -> event -> Either String result@
-- which matches that of `myHandler`.
--
-- @
--     {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
--     module Main where
--
--     import AWS.Lambda.Context (LambdaContext(..))
--     import AWS.Lambda.Runtime (readerTRuntime)
--     import AWS.Lambda.Combinators (withFallibleInterface)
--     import Data.Aeson (FromJSON)
--     import Data.Text (unpack)
--     import GHC.Generics (Generic)
--
--     data Named = Named {
--       name :: String
--     } deriving Generic
--     instance FromJSON Named
--
--     myHandler :: LambdaContext -> Named -> Either String String
--     myHandler (LambdaContext { functionName }) (Named { name }) =
--       if name == \"World\" then
--         Right $ "Hello, World from " ++ unpack functionName ++ "!"
--       else
--         Left "Can only greet the world."
--
--     main :: IO ()
--     main = (readerTRuntime . withFallibleInterface) myHandler
-- @
withFallibleInterface :: MonadReader c m => (c -> b -> Either String a) -> b -> m a
withFallibleInterface fn event = do
  config <- ask
  dropEither $ fn config event

-- | This combinator takes a handler that accepts both an event and
-- 'AWS.Lambda.Context.LambdaContext' and converts it into a handler that is
-- compatible with the base monadic runtime.
--
-- In the example below, we reconstruct 'AWS.Lambda.Runtime.pureRuntimeWithContext'
-- without actually using it.
-- The 'AWS.Lambda.Runtime.readerTRuntime' expects a handler in the form of
-- @event -> ReaderT LambdaContext IO result@ (ignoring constraints).
-- By composing it with `withPureInterface` we get a new runtime which
-- expects a function in the form of @LambdaContext -> event -> result@
-- which matches that of `myHandler`.
--
-- @
--     {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
--     module Main where
--
--     import AWS.Lambda.Context (LambdaContext(..))
--     import AWS.Lambda.Runtime (readerTRuntime)
--     import AWS.Lambda.Combinators (withPureInterface)
--     import Data.Aeson (FromJSON)
--     import Data.Text (unpack)
--     import GHC.Generics (Generic)
--
--     data Named = Named {
--       name :: String
--     } deriving Generic
--     instance FromJSON Named
--
--     myHandler :: LambdaContext -> Named -> String
--     myHandler (LambdaContext { functionName }) (Named { name }) =
--       "Hello, " ++ name ++ " from " ++ unpack functionName ++ "!"
--
--     main :: IO ()
--     main = (readerTRuntime . withPureInterface) myHandler
-- @
withPureInterface :: MonadReader c m => (c -> b -> a) -> b -> m a
withPureInterface fn event = do
  config <- ask
  return $ fn config event

-- | An alias of 'const', this upgrades a handler that does not accept
-- 'AWS.Lambda.Context.LambdaContext' as its first curried argument to one that does.
--
-- This allows us to use other combinators to construct a lambda runtime that accepts
-- a handler that ignores 'AWS.Lambda.Context.LambdaContext'.
--
-- In the example below, we reconstruct 'AWS.Lambda.Runtime.pureRuntime' without actually using it.
-- The 'AWS.Lambda.Runtime.readerTRuntime' expects a handler in the form of
-- @event -> ReaderT LambdaContext IO result@ (ignoring constraints).
-- By composing it with `withPureInterface` we get a new runtime which
-- expects a function in the form of @LambdaContext -> event -> result@,
-- And then finally we also compose `withoutContext` so it accepts the signature
-- @event -> result@ which matches that of `myHandler`.
--
-- @
--     {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
--     module Main where
--
--     import AWS.Lambda.Runtime (readerTRuntime)
--     import AWS.Lambda.Combinators (withPureInterface, withoutContext)
--     import Data.Aeson (FromJSON)
--     import GHC.Generics (Generic)
--
--     data Named = Named {
--       name :: String
--     } deriving Generic
--     instance FromJSON Named
--
--     myHandler :: Named -> String
--     myHandler (Named { name }) =
--       "Hello, " ++ name
--
--     main :: IO ()
--     main = (readerTRuntime . withPureInterface . withoutContext) myHandler
-- @
withoutContext :: a -> b -> a
withoutContext = const

-- | This modifies a function to accept a JSON AST (Value), instead of its JSON parsable
-- input.  It also assumes that the JSON AST passed in will ALWAYS be convertable into the
-- original input type.
--
-- This allows us to write handlers of the types we're interested in, but then map back
-- to the "native" handler that is only guaranteed JSON (but not necessarily in a useful
-- or restricted structure).
--
-- This is essentially the glue that converts the "AWS.Lambda.Runtime.Value" to
-- (the more standard) "AWS.Lambda.Runtime".  While both export a
-- 'AWS.Lambda.Runtime.mRuntimeWithContext', the difference is that the Value
-- Runtime makes no attempt to convert the JSON AST, the standard Runtime does.
--
-- Rarely would this function be used directly, and you wouldn't want to use it
-- at all, (directly or indirectly via Runtime runtimes), if you wanted to act
-- on a failure to convert the JSON AST sent to the Lambda.
withInfallibleParse :: FromJSON a => (a -> b) -> Value -> b
withInfallibleParse fn = either error fn . parseEither parseJSON
