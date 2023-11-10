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
    withoutContext,
    withInfallibleParse,
    withInfallibleParseEither
) where

import           Data.Aeson       (FromJSON, Value, parseJSON)
import           Data.Aeson.Types (parseEither)

-- | An alias of 'const', this upgrades a handler that does not accept
-- 'AWS.Lambda.Context.LambdaContext' as its first curried argument to one that does.
--
-- This allows us to use other combinators to construct a lambda runtime that accepts
-- a handler that ignores 'AWS.Lambda.Context.LambdaContext'.
--
-- In the example below, we reconstruct 'AWS.Lambda.Runtime.pureRuntime'
-- without actually using it.
-- @
--     {-\# LANGUAGE NamedFieldPuns, DeriveGeneric \#-}
--
--     module Main where
--
--     import AWS.Lambda.Runtime (pureRuntimeWithContext)
--     import AWS.Lambda.Combinators (withoutContext)
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
--     main = (pureRuntimeWithContext . withoutContext) myHandler
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

withInfallibleParseEither :: FromJSON a => (a -> b) -> Value -> Either String b
withInfallibleParseEither fn = fmap fn . parseEither parseJSON
