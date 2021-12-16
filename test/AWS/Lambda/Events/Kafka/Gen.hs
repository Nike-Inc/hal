module AWS.Lambda.Events.Kafka.Gen where

import AWS.Lambda.Events.Kafka              (KafkaEvent(KafkaEvent),
                                             EventSource (..),
                                             Header(..),
                                             Record,
                                             Record'     (Record),
                                             Timestamp   (..),
                                             int64ToUTCTime)
import           Control.Applicative        (liftA2)
import           Data.Char                  (isPrint)
import           Data.Foldable              (fold)
import           Data.List                  (intersperse)
import qualified Data.List.NonEmpty         as NE
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Map                   (Map)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import           Data.Traversable           (for)
import           Data.Vector
import qualified Data.Vector                as V
import           Hedgehog                   (Gen)
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

kafkaEvent :: Gen KafkaEvent
kafkaEvent = KafkaEvent
  <$> eventSource
  <*> Gen.maybe eventSourceArn
  <*> bootstrapServers
  <*> records

eventSource :: Gen EventSource
eventSource = Gen.enumBounded

eventSourceArn :: Gen Text
eventSourceArn = do
  region <- Gen.element ["ap-south-1", "us-east-2", "eu-west-3"]
  account <- Gen.text (Range.singleton 12) Gen.digit
  clusterName <- Gen.text (Range.linear 1 20) printable
  uuidParts <- for [ 7, 4, 4, 4, 12, 2 ] $ \n ->
    Gen.text (Range.singleton n) Gen.hexit
  let uuid = fold $ intersperse "-" uuidParts
      resource = T.intercalate "/" ["cluster", clusterName, uuid]
  pure $ T.intercalate ":" ["arn:aws:kafka", region, account, resource]

bootstrapServers :: Gen (NonEmpty Text)
bootstrapServers = Gen.nonEmpty (Range.linear 1 5) server
  where
    server :: Gen Text
    server = (\h p -> h <> ":" <> p) <$> host <*> port

    host :: Gen Text
    host = fold . NE.intersperse "." <$> domainParts

    domainParts :: Gen (NonEmpty Text)
    domainParts = Gen.nonEmpty (Range.linear 1 5) fragment

    fragment :: Gen Text
    fragment = Gen.text (Range.linear 1 20) .
      Gen.element $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['-', '_']

    port :: Gen Text
    port = T.pack . show <$> Gen.int (Range.linear 0 65535)

records :: Gen (Map Text [Record])
records = Gen.map (Range.exponential 1 3)
  . liftA2 (,) topicKey
  $ Gen.list (Range.linear 1 5) record
  where
    topicKey :: Gen Text
    topicKey = do
      t <- topic
      n <- T.pack . show <$> Gen.int (Range.linear 0 10)
      pure $ t <> "-" <> n

record :: Gen Record
record = Record
  <$> topic
  <*> Gen.int32 (Range.exponential 0 maxBound)
  <*> Gen.int64 (Range.linear 0 maxBound)
  <*> timestamp
  <*> headers
  <*> Gen.maybe (Gen.bytes (Range.exponential 0 512))
  <*> Gen.maybe (Gen.bytes (Range.exponential 0 512))

timestamp :: Gen Timestamp
timestamp = Gen.choice
  [ pure NoTimestampType
  , CreateTime <$> utcTime
  , LogAppendTime <$> utcTime
  ]
  where
    utcTime :: Gen UTCTime
    utcTime = int64ToUTCTime <$> Gen.int64
      (Range.exponentialFrom 1600000000000 1650000000000 1700000000000)

headers :: Gen (Vector Header)
headers = V.fromList <$> Gen.list (Range.linear 0 10)
  (Header
    <$> Gen.text (Range.linear 0 512) Gen.unicode
    <*> Gen.bytes (Range.linear 0 512))

topic :: Gen Text
topic = Gen.text (Range.linear 1 20) printable

printable :: Gen Char
printable = Gen.filter isPrint Gen.ascii
