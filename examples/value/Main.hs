{-# LANGUAGE NamedFieldPuns, DeriveGeneric #-}

module Main where

import AWS.Lambda.Context (LambdaContext(..), runReaderTLambdaContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value, FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Text (unpack)
import System.Environment (getEnv)
import GHC.Generics (Generic)

data Named = Named {
  name :: String
} deriving Generic
instance FromJSON Named

myHandler :: Value -> StateT Int (ReaderT LambdaContext IO) String
myHandler jsonAst =
  case parseMaybe parseJSON jsonAst of
    Nothing -> return $ "My name is HAL, what's yours?"
    Just Named { name } -> do
      LambdaContext { functionName } <- ask
      greeting <- liftIO $ getEnv "GREETING"

      greetingCount <- get
      put $ greetingCount + 1

      return $ greeting ++ name ++ " (" ++ show greetingCount ++ ") from " ++ unpack functionName ++ "!"

main :: IO ()
main = runReaderTLambdaContext (evalStateT (mRuntimeWithContext myHandler) 0)
