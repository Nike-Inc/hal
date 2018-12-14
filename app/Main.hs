{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime     (LambdaContext(..),
                                         runReaderTLambdaContext, mLambdaContextRuntime)
import           Control.Exception.Base (ioError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask)
import           Control.Monad.State    (StateT, get, put, evalStateT)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as M
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
    ("083124926037", "Nike+ Test"),
    ("218741990574", "Nike+ Prod"),
    ("020484671131", "Tools")
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
    Left es    -> ioError $ userError es
    Right acct -> fmap Right $ hPutStrLn stderr $ "Hello, " ++ acct ++ "!"

testEnvHandler :: LambdaContext -> AccountIdEvent -> IO (Either String String)
testEnvHandler ctx _ = do
  return $ Right (show ctx)

testReaderT :: AccountIdEvent -> ReaderT LambdaContext IO String
testReaderT AccountIdEvent { accountId } = do
  LambdaContext { functionName, functionMemorySize } <- ask
  liftIO $ hPutStrLn stderr $ "I hope " ++ functionMemorySize ++ "MB is enough to handle the lookup of " ++ accountId ++ "!"
  case M.lookup accountId knownAccounts of
    Nothing   -> error "Not Found"
    Just acct -> return $ acct ++ " (this account name brought to you by " ++ functionName ++ ")"

testStateT :: AccountIdEvent -> ReaderT LambdaContext (StateT Int IO) String
testStateT AccountIdEvent { accountId } = do
  LambdaContext { functionName } <- ask
  executionNum <- get
  liftIO $ hPutStrLn stderr $ "Execution #" ++ show executionNum ++ " for " ++ functionName
  put $ executionNum + 1
  case M.lookup accountId knownAccounts of
    Nothing   -> error "Not Found"
    Just acct -> return $ acct

main :: IO ()
main = flip evalStateT 1 $ runReaderTLambdaContext $ mLambdaContextRuntime testStateT
