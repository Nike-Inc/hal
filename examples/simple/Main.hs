{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import AWS.Lambda.Runtime (simpleLambdaRuntime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data IdEvent  = IdEvent { input   :: String } deriving Generic
instance FromJSON IdEvent where

data IdResult = IdResult { output :: String } deriving Generic
instance ToJSON IdResult where

handler :: IdEvent -> IdResult
handler IdEvent { input } = IdResult { output = input }

main :: IO ()
main = simpleLambdaRuntime handler
