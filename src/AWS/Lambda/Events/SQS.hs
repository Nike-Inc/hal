{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
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
  MessageAttribute (..),
  MessageAttributeValue (..),
  SQSEvent (..)
) where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Map               (Map)
import           Data.Scientific        (Scientific)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as TE
import           GHC.Generics           (Generic)
import           Text.Read              (readMaybe)

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
  messageGroupId                   :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \v -> do
    approximateReceiveCount <- v .: "ApproximateReceiveCount"
    sentTimestamp <- v .: "SentTimestamp"
    senderId <- v .: "SenderId"
    approximateFirstReceiveTimestamp <- v .: "ApproximateFirstReceiveTimestamp"
    messageGroupId <- v .:? "MessageGroupId"
    pure Attributes {..}

-- | An SQS message attribute as it appears in Lambda SQS event payloads.
--
-- See the <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html#example-standard-queue-message-event AWS Lambda SQS event payload example>
-- for the JSON shape used under @messageAttributes@.
data MessageAttribute = MessageAttribute {
  customTypeLabel :: Maybe Text,
  value           :: MessageAttributeValue
} deriving (Show, Eq, Generic)

instance FromJSON MessageAttribute where
  parseJSON = withObject "MessageAttribute" $ \v -> do
    dataType <- v .: "dataType"
    let (baseType, customTypeLabel) = splitDataType dataType

    value <- case baseType of
      "Binary" -> Binary . decodeBase64Text <$> v .: "binaryValue"
      "Number" -> Number <$> (v .: "stringValue" >>= parseNumber)
      "String" -> String <$> v .: "stringValue"
      _ -> fail $ "Unexpected message attribute dataType: " <> show dataType

    pure MessageAttribute { customTypeLabel, value }
    where
      parseNumber = maybe (fail "can't parse stringValue into Scientific") pure . readMaybe

data MessageAttributeValue
  = Binary ByteString
  | Number Scientific
  | String Text
  deriving (Show, Eq, Generic)

data SQSEvent = SQSEvent {
  messageId         :: Text,
  receiptHandle     :: Text,
  body              :: Text,
  attributes        :: Attributes,
  messageAttributes :: Map Text MessageAttribute,
  md5OfBody         :: Text,
  eventSource       :: Text,
  eventSourceARN    :: Text,
  awsRegion         :: Text
} deriving (Show, Eq, Generic)

instance FromJSON SQSEvent

decodeBase64Text :: Text -> ByteString
decodeBase64Text = B64.decodeLenient . TE.encodeUtf8

splitDataType :: Text -> (Text, Maybe Text)
splitDataType dataType =
  case Text.breakOn "." dataType of
    (baseType, "") -> (baseType, Nothing)
    (baseType, customTypeLabel) ->
      (baseType, Just $ Text.drop 1 customTypeLabel)
