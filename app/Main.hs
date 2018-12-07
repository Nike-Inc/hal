module Main where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic(..))
import AWS.Lambda.Runtime (pureLambdaRuntime)

data HardCodedEvent = HardCodedEvent
  { value  :: Int
  } deriving (Show, Generic)

instance ToJSON HardCodedEvent
instance FromJSON HardCodedEvent

-- Some test functions
handler :: HardCodedEvent -> HardCodedEvent
handler = id

fallibleHandler :: HardCodedEvent -> Either String Int
fallibleHandler _ = Left "I always fail, sucker."

main :: IO ()
main = pureLambdaRuntime fallibleHandler
