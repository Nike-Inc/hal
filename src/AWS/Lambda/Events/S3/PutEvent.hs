{-|
Module      : AWS.Lambda.Events.S3.PutEvent
Description : Data types for working with S3 put events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.S3.PutEvent (
  PrincipalIdentity(..),
  PutEvent(..),
  Records(..),
  RequestParameters(..),
  ResponseElements(..),
  S3Bucket(..),
  S3Config(..),
  S3Object(..)
) where

import Data.Aeson   (FromJSON (..), withObject, (.:))
import Data.Text    (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

newtype Records = Records {
  records :: [PutEvent]
} deriving (Show, Eq)

-- | Define our own parse so that `Records` is normalized to `records`.
instance FromJSON Records where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data S3Object = S3Object {
  eTag      :: Text,
  sequencer :: Text,
  key       :: Text,
  size      :: Int
} deriving (Show, Eq, Generic)

instance FromJSON S3Object

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

data S3Config = S3Config {
  bucket          :: S3Bucket,
  configurationId :: Text,
  object          :: S3Object,
  s3SchemaVersion :: Text
} deriving (Show, Eq, Generic)

instance FromJSON S3Config

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

data PutEvent = PutEvent {
  awsRegion         :: Text,
  eventName         :: Text,
  eventSource       :: Text,
  eventTime         :: UTCTime,
  eventVersion      :: Text,
  requestParameters :: RequestParameters,
  responseElements  :: ResponseElements,
  s3                :: S3Config,
  userIdentity      :: PrincipalIdentity
} deriving (Show, Eq, Generic)

instance FromJSON PutEvent
