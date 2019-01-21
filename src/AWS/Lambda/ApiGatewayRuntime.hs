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
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..))
import           AWS.Lambda.Events.NeedsARealName          (NeedsARealName,
                                                            needsARealNameJSON)
import qualified AWS.Lambda.Runtime                        as Runtime
import           Control.Monad.Catch                       (MonadCatch)
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Reader                      (MonadReader,
                                                            ReaderT, ask,
                                                            runReaderT)
import           Data.Aeson                                (FromJSON)
import           System.Envy                               (defConfig)

mRuntimeWithContext :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON json) =>
  ApiGatewayProxyResponse -> (NeedsARealName json -> m ApiGatewayProxyResponse) -> m ()
mRuntimeWithContext res400 fn =
  Runtime.mRuntimeWithContext wrapped
  where
    wrapped e =
      case needsARealNameJSON e of
        Just json -> fn json
        Nothing   -> return res400

-- | Helper for using arbitrary monads with only the LambdaContext in its Reader
readerTRuntimeWithContext :: ReaderT LambdaContext m a -> m a
readerTRuntimeWithContext = flip runReaderT defConfig

readerTRuntime :: FromJSON json =>
  ApiGatewayProxyResponse -> (NeedsARealName json -> ReaderT LambdaContext IO ApiGatewayProxyResponse) -> IO ()
readerTRuntime res404 = readerTRuntimeWithContext .  mRuntimeWithContext res404


ioRuntimeWithContext :: FromJSON json =>
  ApiGatewayProxyResponse -> (LambdaContext -> NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntimeWithContext res404 fn = readerTRuntime res404 (\event -> do
  config <- ask
  result <- liftIO $ fn config event
  case result of
    Left e  -> error e
    Right x -> return x
 )


ioRuntime :: FromJSON json =>
  ApiGatewayProxyResponse -> (NeedsARealName json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntime res404 fn = ioRuntimeWithContext res404 wrapped
    where wrapped _ = fn


fallibleRuntimeWithContext :: FromJSON json =>
  ApiGatewayProxyResponse -> (LambdaContext -> NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntimeWithContext res404 fn = ioRuntimeWithContext res404 wrapped
  where wrapped c e = return $ fn c e


fallibleRuntime :: FromJSON json =>
  ApiGatewayProxyResponse -> (NeedsARealName json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntime res404 fn = fallibleRuntimeWithContext res404 wrapped
  where
    wrapped _ = fn


pureRuntimeWithContext :: FromJSON json =>
  ApiGatewayProxyResponse -> (LambdaContext -> NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntimeWithContext res404 fn = fallibleRuntimeWithContext res404 wrapped
  where wrapped c e = Right $ fn c e


pureRuntime :: FromJSON json => ApiGatewayProxyResponse -> (NeedsARealName json -> ApiGatewayProxyResponse) -> IO ()
pureRuntime res404 fn = fallibleRuntime res404 (Right . fn)
