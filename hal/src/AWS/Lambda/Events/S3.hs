{-|
Module      : AWS.Lambda.Events.S3
Description : Data types for working with S3 events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

{-# LANGUAGE DuplicateRecordFields #-}

module AWS.Lambda.Events.S3 (
  PrincipalIdentity(..),
  Records(..),
  RequestParameters(..),
  ResponseElements(..),
  S3Bucket(..),
  S3Config(..),
  S3Event(..),
  S3Object(..)
) where

import Data.Aeson       (FromJSON (..), Value (Object), withObject, (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text        (Text)
import Data.Time.Clock  (UTCTime)
import GHC.Generics     (Generic)

newtype Records = Records {
  records :: [S3Event]
} deriving (Show, Eq)

instance FromJSON Records where
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

-- | Event data sent by S3 when triggering a Lambda.
data S3Event = S3Event {
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

instance FromJSON S3Event

-- | S3 object representations based on event type received.
--
-- Currently only Put/Delete events can trigger Lambdas
data S3Object =
  PutObject {
    eTag      :: Text,
    sequencer :: Text,
    key       :: Text,
    size      :: Int
  } | DeleteObject {
    sequencer :: Text,
    key       :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON S3Object where
  parseJSON (Object o) = do
    maybeEtag  <- o .:? "eTag"
    maybeSize  <- o .:? "size"
    key'       <- o .:  "key"
    sequencer' <- o .:  "sequencer"

    return $ case (maybeEtag, maybeSize) of
      (Just etag', Just size') -> PutObject etag' sequencer' key' size'
      _                        -> DeleteObject sequencer' key'

  parseJSON invalid    = typeMismatch "S3Object" invalid
