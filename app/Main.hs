{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime     (LambdaContext (..),
                                         withContext,
                                         mLambdaContextRuntime,
                                         runReaderTLambdaContext, HasLambdaContext, defConfig)
import           Control.Exception.Base (ioError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as M
import           GHC.Generics           (Generic (..))
import           System.IO              (hPutStrLn, stderr)
import           System.IO.Error        (userError)
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import qualified System.Environment as Env
import Data.Maybe (fromMaybe)

data Environment = Environment
  { apiKey :: IORef (Maybe String),
    context   :: LambdaContext
  }

instance HasLambdaContext Environment where
  withContext ctx env = env { context = ctx }

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

ioRefHandler :: AccountIdEvent -> ReaderT Environment IO String
ioRefHandler AccountIdEvent { accountId } = do
  apiKeyRef <- asks apiKey
  LambdaContext { functionName } <- asks context
  mApiKey <- liftIO $ readIORef apiKeyRef

  key <- case mApiKey of
            Nothing -> do
              keyFromEnv <- liftIO $ Env.lookupEnv "API_TOKEN"

              liftIO $ writeIORef apiKeyRef keyFromEnv

              return $ fromMaybe "" keyFromEnv
            Just key -> return key

  liftIO $ hPutStrLn stderr $ key
  liftIO $ hPutStrLn stderr $ functionName

  return accountId

main :: IO ()
main = do
  apiKeyRef <- newIORef Nothing
  let env = Environment { apiKey = apiKeyRef, context = (defConfig :: LambdaContext) }

  runReaderT (mLambdaContextRuntime ioRefHandler) env
