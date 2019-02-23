{-|
Module      : AWS.Lambda.Events.S3.PutEvent
Description : Data types for working with S3 put events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.S3.PutEvent (
  module AWS.Lambda.Events.S3.BaseEvent,
  S3PutObject(..),
  PutRecords
) where

import Data.Aeson   (FromJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)
import AWS.Lambda.Events.S3.BaseEvent

data S3PutObject = S3PutObject {
  eTag      :: Text,
  sequencer :: Text,
  key       :: Text,
  size      :: Int
} deriving (Show, Eq, Generic)

instance FromJSON S3PutObject

type PutRecords = Records S3PutObject
