module AWS.Lambda.Events.ApiGateway.ProxyResponse.Spec where

import qualified AWS.Lambda.Events.ApiGateway.ProxyResponse.Gen as Gen
import           Data.Aeson (decode, encode)
import           Hedgehog
import           Test.Hspec (Spec, describe, specify)
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
    describe "properties" $
        specify "tripping" $
            hedgehog $ do
                request <- forAll Gen.proxyResponse
                tripping request encode decode
