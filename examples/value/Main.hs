{-# LANGUAGE NamedFieldPuns, DeriveGeneric #-}

module Main where

import AWS.Lambda.Context (LambdaContext(..))
import AWS.Lambda.Runtime (mRuntimeWithContext)
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

myHandler :: LambdaContext -> Value -> StateT Int IO String
myHandler LambdaContext { functionName } jsonAst =
  case parseMaybe parseJSON jsonAst of
    Nothing -> return $ "My name is HAL, what's yours?"
    Just Named { name } -> do
      greeting <- liftIO $ getEnv "GREETING"

      greetingCount <- get
      put $ greetingCount + 1

      return $ greeting ++ name ++ " (" ++ show greetingCount ++ ") from " ++ unpack functionName ++ "!"

main :: IO ()
main = evalStateT (mRuntimeWithContext myHandler) 0
