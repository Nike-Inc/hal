module AWS.Lambda.Events.SQS (
  Records (..),
  Attributes (..),
  SQSEvent (..)
) where

import Data.Aeson       (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)

newtype Records = Records {
  records :: [SQSEvent]
} deriving (Show, Eq)

instance FromJSON Records where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data Attributes = Attributes {
  approximateReceiveCount :: Text,
  sentTimestamp :: Text,
  senderId :: Text,
  approximateFirstReceiveTimestamp :: Text
} deriving (Show, Eq)

instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \v ->
    Attributes
      <$> v .: "ApproximateReceiveCount"
      <*> v .: "SentTimestamp"
      <*> v .: "SenderId"
      <*> v .: "ApproximateFirstReceiveTimestamp"

data SQSEvent = SQSEvent {
  messageId :: Text,
  receiptHandle :: Text,
  body :: Text,
  attributes :: Attributes,
  messageAttributes :: Map Text Text,
  md5OfBody :: Text,
  eventSource :: Text,
  eventSourceARN :: Text,
  awsRegion :: Text
} deriving (Show, Eq, Generic)

instance FromJSON SQSEvent

