{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AWS.Lambda.Events.EventBridge.Spec where

import           Prelude                                                    hiding (id)

import           AWS.Lambda.Events.EventBridge
import qualified AWS.Lambda.Events.EventBridge.Gen                          as EventBridge
import qualified AWS.Lambda.Events.EventBridge.SSM.ParameterStoreChange.Gen as ParameterStoreChange
import           Data.Aeson                                                 (decode, eitherDecode, encode,
                                                                             object, (.=), Value(..))
import           Data.ByteString.Lazy                                       (ByteString)
import           Data.Time                                                  (UTCTime(..), fromGregorian,
                                                                             secondsToDiffTime)
import           Hedgehog                                                   (forAll, tripping)
import           Test.Hspec                                                 (Spec, describe, shouldBe, specify)
import           Test.Hspec.Hedgehog                                        (hedgehog)
import           Test.Hspec.Runner                                          (hspec)
import           Text.RawString.QQ                                          (r)

spec :: Spec
spec = do
  describe "EventBridge" $ do
    specify "read sample payload" $
      eitherDecode samplePayload `shouldBe` Right expectedPayload

    describe "properties" $ do
      specify "EventBridgeEvent tripping" $
        hedgehog $ do
          payload <- forAll EventBridge.eventBridgeEvent
          tripping payload encode decode

    describe "SSM" $
      specify "ParameterStoreChange tripping" $
        hedgehog $ do
          change <- forAll ParameterStoreChange.parameterStoreChange
          tripping change encode decode

samplePayload :: ByteString
samplePayload = [r|
{
   "version":"0",
   "id":"01234567-0123-0123-0123-0123456789ab",
   "detail-type":"Maintenance Window Target Registration Notification",
   "source":"aws.ssm",
   "account":"123456789012",
   "time":"2016-11-16T00:58:37Z",
   "region":"us-east-2",
   "resources":[
      "arn:aws:ssm:us-east-2:123456789012:maintenancewindow/mw-0ed7251d3fcf6e0c2",
      "arn:aws:ssm:us-east-2:123456789012:windowtarget/e7265f13-3cc5-4f2f-97a9-7d3ca86c32a6"
   ],
   "detail":{
      "window-target-id":"e7265f13-3cc5-4f2f-97a9-7d3ca86c32a6",
      "window-id":"mw-0ed7251d3fcf6e0c2",
      "status":"REGISTERED"
   }
}
|]

expectedPayload :: EventBridgeEvent
expectedPayload = EventBridgeEvent
  { version = "0"
  , id = "01234567-0123-0123-0123-0123456789ab"
  , detailType = "Maintenance Window Target Registration Notification"
  , source = "aws.ssm"
  , account = "123456789012"
  , time = UTCTime (fromGregorian 2016 11 16) (secondsToDiffTime $ 58 * 60 + 37)
  , region = "us-east-2"
  , resources =
    [ "arn:aws:ssm:us-east-2:123456789012:maintenancewindow/mw-0ed7251d3fcf6e0c2"
    , "arn:aws:ssm:us-east-2:123456789012:windowtarget/e7265f13-3cc5-4f2f-97a9-7d3ca86c32a6"
    ]
  , detail = object
    [ "window-target-id" .= String "e7265f13-3cc5-4f2f-97a9-7d3ca86c32a6"
    , "window-id" .= String "mw-0ed7251d3fcf6e0c2"
    , "status" .= String "REGISTERED"
    ]
  }
