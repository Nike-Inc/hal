{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}

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
import qualified AWS.Lambda.Runtime                        as Runtime
import           Control.Monad.Catch                       (MonadCatch)
import           Control.Monad.IO.Class                    (MonadIO)
import           Control.Monad.Reader                      (MonadReader,
                                                            ReaderT)
import           Data.Aeson                                (FromJSON, decode)
import           Data.Profunctor                           (lmap)
import           Data.Text.Lazy                            (Text)
import           Data.Text.Lazy.Encoding                   (encodeUtf8)

with400 :: Monad m => ApiGatewayProxyResponse -> (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
with400 res400 fn e =
  case e of
    Just json -> fn json
    Nothing   -> return res400

withDefault400 :: Monad m => (a -> m ApiGatewayProxyResponse) -> (Maybe a -> m ApiGatewayProxyResponse)
withDefault400 = with400 (ApiGatewayProxyResponse 400 mempty "Bad Request")

withJSONBody :: FromJSON json => (Maybe (ApiGatewayProxyRequest json) -> a) -> (ApiGatewayProxyRequest Text -> a)
withJSONBody = lmap (\agpr@ApiGatewayProxyRequest { isBase64Encoded, body } ->
    if isBase64Encoded then
      Nothing
    else
      let
        maybeDecoded = decode $ encodeUtf8 body
        setBody b = const b <$> agpr
      in
        fmap setBody maybeDecoded
  )

mRuntimeWithContext :: (HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON json) =>
  (ApiGatewayProxyRequest json -> m ApiGatewayProxyResponse) -> m ()
mRuntimeWithContext =
  Runtime.mRuntimeWithContext . withJSONBody . withDefault400

readerTRuntime :: FromJSON json =>
  (ApiGatewayProxyRequest json -> ReaderT LambdaContext IO ApiGatewayProxyResponse) -> IO ()
readerTRuntime =
  runReaderTLambdaContext . mRuntimeWithContext

ioRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> ApiGatewayProxyRequest json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withIOInterface

ioRuntime :: FromJSON json =>
  (ApiGatewayProxyRequest json -> IO (Either String ApiGatewayProxyResponse)) -> IO ()
ioRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withIOInterface . withoutContext

fallibleRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> ApiGatewayProxyRequest json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withFallibleInterface

fallibleRuntime :: FromJSON json =>
  (ApiGatewayProxyRequest json -> Either String ApiGatewayProxyResponse) -> IO ()
fallibleRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withFallibleInterface . withoutContext

pureRuntimeWithContext :: FromJSON json =>
  (LambdaContext -> ApiGatewayProxyRequest json -> ApiGatewayProxyResponse) -> IO ()
pureRuntimeWithContext =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withPureInterface

pureRuntime :: FromJSON json => (ApiGatewayProxyRequest json -> ApiGatewayProxyResponse) -> IO ()
pureRuntime =
  runReaderTLambdaContext . Runtime.mRuntimeWithContext . withJSONBody . withDefault400 . withPureInterface . withoutContext
