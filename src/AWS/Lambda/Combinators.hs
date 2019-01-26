{-|
Module      : AWS.Lambda.Combinators
Description : Function transformers that can be used to adapt the base runtime into more useful interfaces.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Combinators (
    withIOInterface,
    withFallableInterface,
    withPureInterface,
    withoutContext
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)


-- Helper for converting an either into a monad/exception
dropEither :: Monad m => Either String a -> m a
dropEither = \case 
     Left e  -> error e
     Right x -> return x

withIOInterface :: (MonadReader c m, MonadIO m) => (c -> b -> IO (Either String a)) -> (b -> m a)
withIOInterface fn event = do
  config <- ask
  result <- liftIO $ fn config event
  dropEither result

withFallableInterface :: MonadReader c m => (c -> b -> Either String a) -> b -> m a
withFallableInterface fn event = do
  config <- ask
  dropEither $ fn config event

withPureInterface :: MonadReader c m => (c -> b -> a) -> b -> m a
withPureInterface fn event = do
  config <- ask
  return $ fn config event

withoutContext :: (a -> b) -> (c -> a -> b)
withoutContext = const
