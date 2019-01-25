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

import           AWS.Lambda.Context                        (HasLambdaContext (..),
                                                            LambdaContext (..))
import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..))
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..))
import           AWS.Lambda.Events.NeedsARealName          (NeedsARealName,
                                                            expectJSON,
                                                            needsARealName,
                                                            needsARealNameJSON)
import qualified AWS.Lambda.Runtime                        as Runtime
import           Control.Monad.Catch                       (MonadCatch)
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Reader                      (MonadReader,
                                                            ReaderT, ask,
                                                            runReaderT)
import           Data.Aeson                                (FromJSON)
import           Data.Profunctor                           (lmap)
import           Data.Text.Lazy                            (Text)
import           System.Envy                               (defConfig)

with400 :: Monad m => ApiGatewayProxyResponse -> (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
with400 res400 fn e =
  case e of
    Just json -> fn json
    Nothing   -> return res400

withDefault400 :: Monad m => (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
withDefault400 = with400 (ApiGatewayProxyResponse 400 [] "Bad Request")

withApiGateway :: (NeedsARealName (Bool, Text) -> a) -> (ApiGatewayProxyRequest -> a)
withApiGateway = lmap needsARealName

withJSONBody :: FromJSON json => (Maybe (NeedsARealName json) -> a) -> (NeedsARealName (Bool, Text) -> a)
withJSONBody = lmap expectJSON

mRuntimeWithContext :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON json) =>
  (NeedsARealName json -> m ApiGatewayProxyResponse) -> m ()
mRuntimeWithContext =
  Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400

-- | Helper for using arbitrary monads with only the LambdaContext in its Reader
runReaderTLambdaContext :: ReaderT LambdaContext m a -> m a
runReaderTLambdaContext = flip runReaderT defConfig

readerTRuntime :: FromJSON json =>
  (NeedsARealName json -> ReaderT LambdaContext IO ApiGatewayProxyResponse) -> IO ()
readerTRuntime =
  runReaderTLambdaContext . mRuntimeWithContext

withIOAndContextInterface :: (MonadReader c m, MonadIO m) => (c -> b -> IO (Either String a)) -> (b -> m a)
withIOAndContextInterface fn = \event -> do
   config <- ask
   result <- liftIO $ fn config event
   case result of
     Left e  -> error e
     Right x -> return x


ioRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withIOAndContextInterface

withIOInterface :: MonadIO m => (b -> IO (Either String a)) -> b -> m a
withIOInterface fn event = do
   result <- liftIO $ fn event
   case result of
     Left e  -> error e
     Right x -> return x

ioRuntime :: FromJSON json =>
  (NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withIOInterface

withFallableAndContextInterface :: MonadReader c m => (c -> b -> Either String a) -> b -> m a
withFallableAndContextInterface fn event = do
  config <- ask
  case fn config event of
    Left e  -> error e
    Right x -> return x

fallibleRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withFallableAndContextInterface


withFallableInterface :: Monad m => (b -> Either String a) -> b -> m a
withFallableInterface fn event =
  case fn event of
    Left e  -> error e
    Right x -> return x

fallibleRuntime :: FromJSON json =>
  (NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withFallableInterface


withPureAndContextInterface :: MonadReader c m => (c -> b -> a) -> b -> m a
withPureAndContextInterface fn event = do
  config <- ask
  return $ fn config event

pureRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withPureAndContextInterface


withPureInterface :: Monad m => (b -> a) -> b -> m a
withPureInterface =
  fmap return

pureRuntime :: FromJSON json => (NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withApiGateway . withJSONBody . withDefault400 . withPureInterface
