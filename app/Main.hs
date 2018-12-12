{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime     (ioLambdaRuntime, LambdaContext)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict as M
import           Control.Exception.Base (ioError)
import           GHC.Generics           (Generic (..))
import           System.IO              (hPutStrLn, stderr)
import           System.IO.Error        (userError)

data AccountIdEvent = AccountIdEvent {
  accountId :: String
} deriving (Show, Generic)

instance ToJSON AccountIdEvent
instance FromJSON AccountIdEvent

knownAccounts :: HashMap String String
knownAccounts = M.fromList [
    ("***REMOVED***", "***REMOVED***"),
    ("***REMOVED***", "***REMOVED***"),
    ("***REMOVED***", "***REMOVED***")
  ]

awsAccountHandler :: AccountIdEvent -> Either String String
awsAccountHandler AccountIdEvent { accountId } =
  case M.lookup accountId knownAccounts of
    Nothing   -> Left "Not Found"
    Just acct -> Right acct

-- Note that you have to write to stderr to get into Cloudwatch
printHelloHandler :: AccountIdEvent -> IO (Either String ())
printHelloHandler event =
  case awsAccountHandler event of
    Left es -> ioError $ userError es
    Right acct -> fmap Right $ hPutStrLn stderr $ "Hello, " ++ acct ++ "!"

testEnvHandler :: LambdaContext -> AccountIdEvent -> IO (Either String String)
testEnvHandler ctx _ = do
  return $ Right (show ctx)

main :: IO ()
main = ioLambdaRuntime testEnvHandler
