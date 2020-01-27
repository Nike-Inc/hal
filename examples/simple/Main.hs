{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           AWS.Lambda.Runtime (pureRuntime)
import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)

data IdEvent  = IdEvent { input   :: String } deriving Generic
instance FromJSON IdEvent where

data IdResult = IdResult { output :: String } deriving Generic
instance ToJSON IdResult where

handler :: IdEvent -> IdResult
handler IdEvent { input } = IdResult { output = input }

main :: IO ()
main = pureRuntime handler
