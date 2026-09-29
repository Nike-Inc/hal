{-# LANGUAGE QuasiQuotes #-}

module AWS.Lambda.Events.SQS.Spec where

import           AWS.Lambda.Events.SQS
import           Data.Aeson            (eitherDecode)
import           Data.Either           (isLeft)
import qualified Data.Map              as M
import           Data.ByteString.Lazy  (ByteString)
import           Test.Hspec            (Spec, shouldBe, shouldSatisfy, specify)
import           Text.RawString.QQ     (r)

spec :: Spec
spec = do
  specify "read sample payload" $
    eitherDecode samplePayload `shouldBe` Right expectedRecords

  specify "fail on invalid number message attributes" $
    (eitherDecode invalidNumberPayload :: Either String Records) `shouldSatisfy` isLeft

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
        "attribute1": {
          "stringValue": "value1",
          "stringListValues": ["ignored-string-list-value"],
          "binaryListValues": ["aWdub3JlZC1iaW5hcnktbGlzdC12YWx1ZQ=="],
          "dataType": "String"
        },
        "attribute2": {
          "stringValue": "ignored-string-value",
          "binaryValue": "dmFsdWUy",
          "stringListValues": [],
          "binaryListValues": [],
          "dataType": "Binary"
        },
        "attribute3": {
          "stringValue": "123.45",
          "stringListValues": [],
          "binaryListValues": [],
          "dataType": "Number"
        },
        "attribute4": {
          "stringValue": "value4",
          "binaryValue": "aWdub3JlZC1iaW5hcnktdmFsdWU=",
          "stringListValues": ["ignored-extra-string"],
          "binaryListValues": ["aWdub3JlZC1leHRyYS1iaW5hcnk="],
          "dataType": "String.foo"
        }
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
              [ ( "attribute1"
                , MessageAttribute
                    { customTypeLabel = Nothing
                    , value = String "value1"
                    }
                )
              , ( "attribute2"
                , MessageAttribute
                    { customTypeLabel = Nothing
                    , value = Binary "value2"
                    }
                )
              , ( "attribute3"
                , MessageAttribute
                    { customTypeLabel = Nothing
                    , value = Number 123.45
                    }
                )
              , ( "attribute4"
                , MessageAttribute
                    { customTypeLabel = Just "foo"
                    , value = String "value4"
                    }
                )
              ]
          , md5OfBody = "9a0364b9e99bb480dd25e1f0284c8555"
          , eventSource = "aws:sqs"
          , eventSourceARN = "arn:aws:sqs:us-east-1:123456789012:queue1"
          , awsRegion = "us-east-1"
          }
      ]
  }

invalidNumberPayload :: ByteString
invalidNumberPayload = [r|
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
        "ApproximateFirstReceiveTimestamp": "1523232000001"
      },
      "messageAttributes": {
        "attribute1": {
          "stringValue": "not-a-number",
          "dataType": "Number"
        }
      },
      "md5OfBody": "9a0364b9e99bb480dd25e1f0284c8555",
      "eventSource": "aws:sqs",
      "eventSourceARN": "arn:aws:sqs:us-east-1:123456789012:queue1",
      "awsRegion": "us-east-1"
    }
  ]
}
|]
