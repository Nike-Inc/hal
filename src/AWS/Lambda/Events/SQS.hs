{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : AWS.Lambda.Events.SQS
Description : Data types for working with SQS events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.SQS (
  Records (..),
  Attributes (..),
  SQSEvent (..)
) where

import Data.Aeson   (FromJSON (..), withObject, (.:))
import Data.Map     (Map)
import Data.Text    (Text)
import GHC.Generics (Generic)

-- | Represents an event from AWS SQS.
--
-- See the <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html AWS documentation>
-- for a sample payload.
newtype Records = Records {
  records :: [SQSEvent]
} deriving (Show, Eq, Generic)

instance FromJSON Records where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data Attributes = Attributes {
  approximateReceiveCount          :: Text,
  sentTimestamp                    :: Text,
  senderId                         :: Text,
  approximateFirstReceiveTimestamp :: Text,
  messageGroupId                   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \v -> do
    approximateReceiveCount <- v .: "ApproximateReceiveCount"
    sentTimestamp <- v .: "SentTimestamp"
    senderId <- v .: "SenderId"
    approximateFirstReceiveTimestamp <- v .: "ApproximateFirstReceiveTimestamp"
    messageGroupId <- v .: "MessageGroupId"
    pure Attributes {..}

data SQSEvent = SQSEvent {
  messageId         :: Text,
  receiptHandle     :: Text,
  body              :: Text,
  attributes        :: Attributes,
  messageAttributes :: Map Text Text,
  md5OfBody         :: Text,
  eventSource       :: Text,
  eventSourceARN    :: Text,
  awsRegion         :: Text
} deriving (Show, Eq, Generic)

instance FromJSON SQSEvent
