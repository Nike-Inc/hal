module AWS.Lambda.Events.ApiGateway.ProxyRequest.Spec where

import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen as Gen
import           Data.Aeson (decode, encode)
import           Hedgehog
import           Test.Hspec (Spec, describe, specify)
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
    describe "properties" $
        specify "tripping" $
            hedgehog $ do
                request <- forAll Gen.proxyRequest
                tripping request encode decode
