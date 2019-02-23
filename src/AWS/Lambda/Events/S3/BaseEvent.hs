{-|
Module      : AWS.Lambda.Events.S3.BaseEvent
Description : Data types for working with S3 events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.S3.BaseEvent (
  PrincipalIdentity(..),
  BaseEvent(..),
  Records(..),
  RequestParameters(..),
  ResponseElements(..),
  S3Bucket(..),
  S3Config(..)
) where

import Data.Aeson   (FromJSON (..), withObject, (.:))
import Data.Text    (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

newtype Records a = Records {
  records :: [BaseEvent a]
} deriving (Show, Eq)

instance (FromJSON a) => FromJSON (Records a) where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data PrincipalIdentity = PrincipalIdentity {
  principalId :: Text
} deriving (Show, Eq, Generic)

instance FromJSON PrincipalIdentity

data S3Bucket = S3Bucket {
  arn           :: Text,
  name          :: Text,
  ownerIdentity :: PrincipalIdentity
} deriving (Show, Eq, Generic)

instance FromJSON S3Bucket

data S3Config a = S3Config {
  bucket          :: S3Bucket,
  configurationId :: Text,
  object          :: a,
  s3SchemaVersion :: Text
} deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (S3Config a)

data ResponseElements = ResponseElements {
  amazonId        :: Text,
  amazonRequestId :: Text
} deriving (Show, Eq)

instance FromJSON ResponseElements where
  parseJSON = withObject  "ResponseElements" $ \v ->
    ResponseElements <$> v .: "x-amz-id-2" <*> v .: "x-amz-request-id"

data RequestParameters = RequestParameters {
  sourceIPAddress :: Text
} deriving (Show, Eq, Generic)

instance FromJSON RequestParameters

data BaseEvent a = BaseEvent {
  awsRegion         :: Text,
  eventName         :: Text,
  eventSource       :: Text,
  eventTime         :: UTCTime,
  eventVersion      :: Text,
  requestParameters :: RequestParameters,
  responseElements  :: ResponseElements,
  s3                :: S3Config a,
  userIdentity      :: PrincipalIdentity
} deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (BaseEvent a)
