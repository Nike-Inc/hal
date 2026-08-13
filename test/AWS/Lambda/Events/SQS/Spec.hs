{-# LANGUAGE QuasiQuotes #-}

module AWS.Lambda.Events.SQS.Spec where

import           AWS.Lambda.Events.SQS
import           Data.Aeson            (eitherDecode)
import qualified Data.Map              as M
import           Data.ByteString.Lazy  (ByteString)
import           Test.Hspec            (Spec, shouldBe, specify)
import           Text.RawString.QQ     (r)

spec :: Spec
spec =
  specify "read sample payload" $
    eitherDecode samplePayload `shouldBe` Right expectedRecords

samplePayload :: ByteString
samplePayload = [r|
{
  "Records": [
    {
      "messageId": "11111111-2222-3333-4444-555555555555",
      "receiptHandle": "AQEBwJnKyrHigUMZj6rYigCgxlaS3SLy0a...",
      "body": "Hello from SQS!",
      "attributes": {
        "ApproximateReceiveCount": "1",
        "SentTimestamp": "1523232000000",
        "SenderId": "123456789012",
        "ApproximateFirstReceiveTimestamp": "1523232000001",
        "MessageGroupId": "group-1"
      },
      "messageAttributes": {
        "attribute1": "value1",
        "attribute2": "value2"
      },
      "md5OfBody": "9a0364b9e99bb480dd25e1f0284c8555",
      "eventSource": "aws:sqs",
      "eventSourceARN": "arn:aws:sqs:us-east-1:123456789012:queue1",
      "awsRegion": "us-east-1"
    }
  ]
}
|]

expectedRecords :: Records
expectedRecords = Records
  { records =
      [ SQSEvent
          { messageId = "11111111-2222-3333-4444-555555555555"
          , receiptHandle = "AQEBwJnKyrHigUMZj6rYigCgxlaS3SLy0a..."
          , body = "Hello from SQS!"
          , attributes = Attributes
              { approximateReceiveCount = "1"
              , sentTimestamp = "1523232000000"
              , senderId = "123456789012"
              , approximateFirstReceiveTimestamp = "1523232000001"
              , messageGroupId = Just "group-1"
              }
          , messageAttributes = M.fromList
              [ ("attribute1", "value1")
              , ("attribute2", "value2")
              ]
          , md5OfBody = "9a0364b9e99bb480dd25e1f0284c8555"
          , eventSource = "aws:sqs"
          , eventSourceARN = "arn:aws:sqs:us-east-1:123456789012:queue1"
          , awsRegion = "us-east-1"
          }
      ]
  }
