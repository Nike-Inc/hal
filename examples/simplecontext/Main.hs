{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Text (Text)
import           AWS.Lambda.Context (LambdaContext (..))
import           AWS.Lambda.Runtime (pureRuntimeWithContext)
import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)

data IdEvent  = IdEvent { input   :: Text } deriving Generic
instance FromJSON IdEvent where

data FunctionNameResult = FunctionNameResult { output :: Text } deriving Generic
instance ToJSON FunctionNameResult where

-- | All LambdaContext fields can be found in the `AWS.Lambda.Context` module.
handler :: LambdaContext -> IdEvent -> FunctionNameResult
handler LambdaContext { functionName } _ =
  FunctionNameResult { output = functionName }

main :: IO ()
main = pureRuntimeWithContext handler
