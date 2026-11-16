{-# LANGUAGE QuasiQuotes #-}

module AWS.Lambda.Events.ApiGateway.ProxyRequest.Spec where

import           AWS.Lambda.Events.ApiGateway.ProxyRequest (ClientCert)
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen as Gen
import           Data.Aeson (decode, encode, toJSON)
import           Hedgehog
import           Test.Hspec (Spec, describe, shouldBe, specify)
import           Test.Hspec.Hedgehog (hedgehog)
import           Text.RawString.QQ (r)

spec :: Spec
spec = do
    -- AWS's documented example.  Re-encoding it must reproduce the dates
    -- exactly, including the space-padded day.
    specify "decodes and re-encodes a client certificate" $ do
        let cert = [r|{
              "clientCertPem": "CERT_CONTENT",
              "subjectDN": "www.example.com",
              "issuerDN": "Example issuer",
              "serialNumber": "a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1:a1",
              "validity": {
                "notBefore": "May 28 12:30:02 2019 GMT",
                "notAfter": "Aug  5 09:36:04 2021 GMT"
              }
            }|]
        (toJSON <$> (decode cert :: Maybe ClientCert)) `shouldBe` decode cert

    describe "properties" $
        specify "tripping" $
            hedgehog $ do
                request <- forAll Gen.proxyRequest
                tripping request encode decode
