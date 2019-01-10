{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime   (fallibleRuntime)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as M

data AwsAccountRequest = AwsAccountRequest { accountId :: String }
  deriving Generic
instance FromJSON AwsAccountRequest

data AccountNameResponse = AccountNameResponse { accountName :: String }
  deriving Generic
instance ToJSON AccountNameResponse

knownAccounts :: HashMap String String
knownAccounts = M.fromList [
    ("111111111111", "Test Environment"),
    ("222222222222", "Staging Environment"),
    ("333333333333", "Production Environment")
  ]

handler :: AwsAccountRequest -> Either String AccountNameResponse
handler AwsAccountRequest { accountId } =
  case M.lookup accountId knownAccounts of
    Nothing   -> Left "Not Found"
    Just acct -> Right AccountNameResponse { accountName = acct }

main :: IO ()
main = fallibleRuntime handler
