{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : AWS.Lambda.ApiGatewayRuntime
Description : Runtime methods useful when constructing Haskell handlers for the AWS Lambda Custom Runtime.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.ApiGatewayRuntime (
  pureRuntime,
  pureRuntimeWithContext,
  fallibleRuntime,
  fallibleRuntimeWithContext,
  ioRuntime,
  ioRuntimeWithContext,
  readerTRuntime,
  mRuntimeWithContext
) where

import           AWS.Lambda.Combinators                    (withFallibleInterface,
                                                            withIOInterface,
                                                            withPureInterface,
                                                            withoutContext)
import           AWS.Lambda.Context                        (HasLambdaContext (..),
                                                            LambdaContext (..),
                                                            runReaderTLambdaContext)
import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..))
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..))
import           AWS.Lambda.Events.NeedsARealName          (NeedsARealName,
                                                            expectJSON,
                                                            needsARealName)
import qualified AWS.Lambda.Runtime                        as Runtime
import           Control.Monad.Catch                       (MonadCatch)
import           Control.Monad.IO.Class                    (MonadIO)
import           Control.Monad.Reader                      (MonadReader,
                                                            ReaderT)
import           Data.Aeson                                (FromJSON)
import           Data.Profunctor                           (lmap)
import           Data.Text.Lazy                            (Text)

with400 :: Monad m => ApiGatewayProxyResponse -> (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
with400 res400 fn e =
  case e of
    Just json -> fn json
    Nothing   -> return res400

withDefault400 :: Monad m => (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
withDefault400 = with400 (ApiGatewayProxyResponse 400 mempty "Bad Request")

withApiGateway :: (NeedsARealName (Bool, Text) -> a) -> (ApiGatewayProxyRequest -> a)
withApiGateway = lmap needsARealName

withJSONBody :: FromJSON json => (Maybe (NeedsARealName json) -> a) -> (NeedsARealName (Bool, Text) -> a)
withJSONBody = lmap expectJSON

mRuntimeWithContext :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON json) =>
  (NeedsARealName json -> m ApiGatewayProxyResponse) -> m ()
mRuntimeWithContext =
  Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400

readerTRuntime :: FromJSON json =>
  (NeedsARealName json -> ReaderT LambdaContext IO ApiGatewayProxyResponse) -> IO ()
readerTRuntime =
  runReaderTLambdaContext . mRuntimeWithContext

ioRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withIOInterface

ioRuntime :: FromJSON json =>
  (NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withIOInterface . withoutContext

fallibleRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withFallibleInterface

fallibleRuntime :: FromJSON json =>
  (NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withFallibleInterface . withoutContext

pureRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withPureInterface

pureRuntime :: FromJSON json => (NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withPureInterface . withoutContext
