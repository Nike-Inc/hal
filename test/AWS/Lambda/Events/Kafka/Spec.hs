{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AWS.Lambda.Events.Kafka.Spec where

import           AWS.Lambda.Events.Kafka
import qualified AWS.Lambda.Events.Kafka.Gen as Gen
import           Data.Aeson                  (decode, eitherDecode, encode)
import           Data.ByteString.Lazy        (ByteString)
import           Data.Int                    (Int64)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import           Data.Time                   (UTCTime(..), fromGregorian,
                                              picosecondsToDiffTime)
import           Hedgehog                    (forAll, tripping)
import           Test.Hspec                  (Spec, describe, shouldBe, specify)
import           Test.Hspec.Hedgehog         (hedgehog)
import           Text.RawString.QQ           (r)

spec :: Spec
spec = do
    specify "read MSK payload" $
        eitherDecode sampleMskPayload
            `shouldBe` Right expectedMskPayload

    specify "read Self-Managed Kafka payload" $
        eitherDecode sampleSelfManagedKafkaPayload
            `shouldBe` Right expectedSelfManagedKafkaPayload

    describe "properties" $ do
        specify "tripping" $
            hedgehog $ do
                request <- forAll Gen.kafkaEvent
                tripping request encode decode

sampleMskPayload :: ByteString
sampleMskPayload = [r|
{
    "bootstrapServers": "b-1.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9094,b-2.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9094",
    "eventSource": "aws:kafka",
    "eventSourceArn": "arn:aws:kafka:us-east-1:123456789012:cluster/msk-test-cluster-2/669b9b55-48db-44e6-9c75-b9b9ab79ae36-14",
    "records": {
        "welcome-to-the-space-jam-0": [
            {
                "key": "c2xhbQ==",
                "offset": 1,
                "partition": 0,
                "headers": [
                    {
                        "headerKey": [
                            104,
                            101,
                            97,
                            100,
                            101,
                            114,
                            86,
                            97,
                            108,
                            117,
                            101
                        ]
                    }
                ],
                "timestamp": 1621304398263,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            }
        ]
    }
}
|]

expectedMskPayload :: KafkaEvent
expectedMskPayload = KafkaEvent
  { eventSource = AwsKafka
  , eventSourceArn = Just "arn:aws:kafka:us-east-1:123456789012:cluster/msk-test-cluster-2/669b9b55-48db-44e6-9c75-b9b9ab79ae36-14"
  , bootstrapServers = NE.fromList
    [ "b-1.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9094"
    , "b-2.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9094"
    ]
  , records = M.fromList
    [ ( "welcome-to-the-space-jam-0"
      , [ withHeaders [Header "headerKey" "headerValue"]
          . slamJamRecord 1
          $ UTCTime
              (fromGregorian 2021 05 18)
              (picosecondsToDiffTime 8398263000000000)
        ]
      )
    ]
  }

sampleSelfManagedKafkaPayload :: ByteString
sampleSelfManagedKafkaPayload = [r|
{
    "bootstrapServers": "b-2.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9092,b-1.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9092",
    "eventSource": "SelfManagedKafka",
    "records": {
        "welcome-to-the-space-jam-0": [
            {
                "key": "c2xhbQ==",
                "offset": 0,
                "partition": 0,
                "headers": [],
                "timestamp": 1621302644353,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            },
            {
                "key": "c2xhbQ==",
                "offset": 1,
                "partition": 0,
                "headers": [],
                "timestamp": 1621304398263,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            },
            {
                "key": "c2xhbQ==",
                "offset": 2,
                "partition": 0,
                "headers": [],
                "timestamp": 1621309340017,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            },
            {
                "key": "c2xhbQ==",
                "offset": 3,
                "partition": 0,
                "headers": [],
                "timestamp": 1621309352813,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            },
            {
                "key": "c2xhbQ==",
                "offset": 4,
                "partition": 0,
                "headers": [],
                "timestamp": 1621309372115,
                "timestampType": "CREATE_TIME",
                "topic": "welcome-to-the-space-jam",
                "value": "amFt"
            }
        ]
    }
}
|]

expectedSelfManagedKafkaPayload :: KafkaEvent
expectedSelfManagedKafkaPayload = KafkaEvent
  { eventSource = SelfManagedKafka
  , eventSourceArn = Nothing
  , bootstrapServers = NE.fromList
    [ "b-2.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9092"
    , "b-1.msk-test-cluster-2.4jp6ci.c14.kafka.us-east-1.amazonaws.com:9092"
    ]
  , records = M.fromList
    [ ( "welcome-to-the-space-jam-0"
      , [ slamJamRecord 0 $ UTCTime
            (fromGregorian 2021 05 18)
            (picosecondsToDiffTime 6644353000000000)
        , slamJamRecord 1 $ UTCTime
            (fromGregorian 2021 05 18)
            (picosecondsToDiffTime 8398263000000000)
        , slamJamRecord 2 $ UTCTime
            (fromGregorian 2021 05 18)
            (picosecondsToDiffTime 13340017000000000)
        , slamJamRecord 3 $ UTCTime
            (fromGregorian 2021 05 18)
            (picosecondsToDiffTime 13352813000000000)
        , slamJamRecord 4 $ UTCTime
            (fromGregorian 2021 05 18)
            (picosecondsToDiffTime 13372115000000000)
        ]
      )
    ]
  }

slamJamRecord :: Int64 -> UTCTime -> Record
slamJamRecord off stamp = Record
  { topic = "welcome-to-the-space-jam"
  , partition = 0
  , offset = off
  , headers = []
  , timestamp = CreateTime stamp
  , key = Just "slam"
  , value = Just "jam"
  }

withHeaders :: [Header] -> Record -> Record
withHeaders headers record = record { headers }
