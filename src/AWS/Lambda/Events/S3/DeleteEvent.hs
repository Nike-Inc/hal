{-|
Module      : AWS.Lambda.Events.S3.DeleteEvent
Description : Data types for working with S3 delete events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.S3.DeleteEvent (
  module AWS.Lambda.Events.S3.BaseEvent,
  S3DeleteObject(..),
  DeleteRecords
) where

import Data.Aeson   (FromJSON (..))
import Data.Text    (Text)
import GHC.Generics (Generic)
import AWS.Lambda.Events.S3.BaseEvent

data S3DeleteObject = S3DeleteObject {
  sequencer :: Text,
  key       :: Text
} deriving (Show, Eq, Generic)

instance FromJSON S3DeleteObject

type DeleteRecords = Records S3DeleteObject
