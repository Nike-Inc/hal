module AWS.Lambda.Events.S3.PutEvent (
  PutEvent(..)
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), withObject)

newtype Records = Records {
  records :: [PutEvent]
} deriving (Show, Eq)

instance FromJSON Records where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data PutEvent = PutEvent {
  userIdentity :: AmazonIdentity,
  requestParameters :: Record
} deriving (Show, Eq, Generic)

instance FromJSON PutEvent

data S3Object = S3Object {
  eTag      :: Text,
  sequencer :: Text,
  key       :: Text,
  size      :: Int
} deriving (Show, Eq, Generic)

instance FromJSON S3Object

data AmazonIdentity = AmazonIdentity {
  principalId :: Text
} deriving (Show, Eq, Generic)

instance FromJSON AmazonIdentity

data S3Bucket = S3Bucket {
  arn           :: Text,
  name          :: Text,
  ownerIdentity :: AmazonIdentity,
  object        :: S3Object
} deriving (Show, Eq, Generic)

instance FromJSON S3Bucket

data S3Config = S3Config {
  configurationId :: Text,
  bucket          :: S3Bucket,
  s3SchemaVersion :: Text
} deriving (Show, Eq, Generic)

instance FromJSON S3Config

data ResponseElements = ResponseElements {
  amazonId :: Text,
  amazonRequestId :: Text
} deriving (Show, Eq)

instance FromJSON ResponseElements where
  parseJSON = withObject  "ResponseElements" $ \v ->
    ResponseElements <$> v .: "x-amz-id-2" <*> v .: "x-amz-request-id"


data Record = Record {
  sourceIpAddress  :: Text,
  eventVersion     :: Text,
  eventTime        :: Text, -- Should be date
  s3               :: S3Config,
  responseElements :: ResponseElements,
  awsRegion        :: Text,
  eventSource      :: Text,
  eventName        :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Record

{-
Object (fromList [
  ("Records", Array [
    Object (fromList [
      ("userIdentity",Object (fromList [
        ("principalId",String "EXAMPLE")])),
      ("requestParameters",Object (fromList [
        ("sourceIPAddress",String "127.0.0.1")])),
        ("eventVersion",String "2.0"),
        ("responseElements",Object (fromList [
          ("x-amz-id-2",String "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH"),
          ("x-amz-request-id",String "EXAMPLE123456789")])),
          ("eventTime",String "1970-01-01T00:00:00.000Z"),
          ("awsRegion",String "us-east-1"),
          ("s3",Object (fromList [
            ("configurationId",String "testConfigRule"),
            ("bucket",Object (fromList [
              ("ownerIdentity",Object (fromList [
                ("principalId",String "EXAMPLE")])),
              ("arn",String "arn:aws:s3:::foo"),
              ("name",String "foo")])),
              ("object",Object (fromList [
                ("eTag",String "0123456789abcdef0123456789abcdef"),
                ("size",Number 1024.0),
                ("key",String "woaw"),
                ("sequencer",String "0A1B2C3D4E5F678901")])),
              ("s3SchemaVersion",String "1.0")])),
          ("eventName",String "ObjectCreated:Put"),
          ("eventSource",String "aws:s3")])])])
-}
