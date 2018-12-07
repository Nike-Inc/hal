{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime  (pureLambdaRuntime)
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           GHC.Generics        (Generic (..))

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

main :: IO ()
main = pureLambdaRuntime awsAccountHandler
