{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : AWS.Lambda.Events.EventBridge
Description : Data types for consuming EventBridge events.
License     : BSD3
Stability   : stable

Data types for Lambda functions which subscribe to AWS EventBridge
events.

-}

module AWS.Lambda.Events.EventBridge (
  EventBridgeEvent,
  EventBridgeEvent'(..)
) where

import Control.Monad  (unless)
import Data.Aeson
import Data.Text      (Text)
import Data.Time      (UTCTime)
import GHC.Generics   (Generic)
import Prelude        hiding (id)

-- | Convenience alias for events of unknown type. Most of the time
-- you will want to define or use a data type @Foo@ representing the
-- @detail@ payload, and write an @instance FromJSON Foo@ to get
-- @instance FromJSON ('EventBridgeEvent'' Foo)@. See the
-- @AWS.Lambda.Events.EventBridge.Detail.*@ modules for data types
-- which you can use.
type EventBridgeEvent = EventBridgeEvent' Value

-- | Represents an event from Amazon EventBridge.
--
-- See the <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-events.html AWS documentation>
-- for information about events, and a sample payload.
data EventBridgeEvent' a = EventBridgeEvent {
  -- | By default, this is set to 0 (zero) in all events.
  version :: Text,

  -- | A Version 4 UUID that's generated for every event. You can use
  -- id to trace events as they move through rules to targets.
  id :: Text,

  -- | Identifies, in combination with the source field, the fields
  -- and values that appear in the detail field.
  --
  -- Events that are delivered by CloudTrail have AWS API Call via
  -- CloudTrail as the value for detail-type.
  --
  -- __NOTE:__ This is called @detail-type@ in the AWS payload.
  detailType :: Text,

  -- | Identifies the service that generated the event. All events
  -- that come from AWS services begin with "aws." Customer-generated
  -- events can have any value here, as long as it doesn't begin with
  -- "aws." We recommend the use of Java package-name style reverse
  -- domain-name strings.
  --
  -- To find the correct value for source for an AWS service, see The
  -- condition keys table, select a service from the list, and look
  -- for the service prefix. For example, the source value for Amazon
  -- CloudFront is aws.cloudfront.
  source :: Text,

  -- | The 12-digit number identifying an AWS account.
  account :: Text,

  -- | The event timestamp, which can be specified by the service
  -- originating the event. If the event spans a time interval, the
  -- service can report the start time, so this value might be before
  -- the time the event is received.
  time :: UTCTime,

  -- | Identifies the AWS Region where the event originated.
  region :: Text,

  -- | A JSON array that contains ARNs that identify resources that
  -- are involved in the event. The service generating the event
  -- determines whether to include these ARNs. For example, Amazon EC2
  -- instance state-changes include Amazon EC2 instance ARNs, Auto
  -- Scaling events include ARNs for both instances and Auto Scaling
  -- groups, but API calls with AWS CloudTrail do not include resource
  -- ARNs.
  resources :: [Text],

  -- | A JSON object that contains information about the event. The
  -- service generating the event determines the content of this
  -- field. The detail content can be as simple as two fields. AWS API
  -- call events have detail objects with approximately 50 fields
  -- nested several levels deep.
  detail :: a
} deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON a => FromJSON (EventBridgeEvent' a) where
  parseJSON = withObject "EventBridgeEvent" $ \o -> do
    version <- o .: "version"
    unless (version == "0") $ fail "version != 0"
    id <- o .: "id"
    detailType <- o .: "detail-type"
    source <- o .: "source"
    account <- o .: "account"
    time <- o .: "time"
    region <- o .: "region"
    resources <- o .: "resources"
    detail <- o .: "detail"

    pure EventBridgeEvent
      { version
      , id
      , detailType
      , source
      , account
      , time
      , region
      , resources
      , detail
      }

instance ToJSON a => ToJSON (EventBridgeEvent' a) where
  toJSON event = object
    [ "version" .= version event
    , "id" .= id event
    , "detail-type" .= detailType event
    , "source" .= source event
    , "account" .= account event
    , "time" .= time event
    , "region" .= region event
    , "resources" .= resources event
    , "detail" .= detail event
    ]

  toEncoding event = pairs $ mconcat
    [ "version" .= version event
    , "id" .= id event
    , "detail-type" .= detailType event
    , "source" .= source event
    , "account" .= account event
    , "time" .= time event
    , "region" .= region event
    , "resources" .= resources event
    , "detail" .= detail event
    ]
