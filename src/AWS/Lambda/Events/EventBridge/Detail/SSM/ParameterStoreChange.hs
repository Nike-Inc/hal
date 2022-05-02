{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange
-- Description : Data types for AWS Systems Manager Parameter Store Change events.
-- License     : BSD3
-- Stability   : stable
module AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange
  ( ParameterStoreChange (..),
    Operation (.., Create, Update, Delete, LabelParameterVersion),
    Type (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    pairs,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Encoding (text)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A @Parameter Store Change@ event from Amazon EventBridge. This
-- structure corresponds to the contents of the @"detail"@ field of an
-- EventBridge event, so a full payload from EventBridge can be parsed
-- into a @'AWS.Lambda.Events.EventBridge'' ParameterStoreChange@.
--
-- Sample event payloads are provided in the
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-systems-manager-event-examples.html#SSM-Parameter-Store-event-types AWS Systems Manager User Guide>.
data ParameterStoreChange = ParameterStoreChange
  { operation :: Operation,
    name :: Text,
    type_ :: Type,
    description :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ParameterStoreChange where
  parseJSON = withObject "ParameterStoreChange" $ \o -> do
    operation <- o .: "operation"
    name <- o .: "name"
    type_ <- o .: "type"
    description <- o .:? "description"

    pure
      ParameterStoreChange
        { operation,
          name,
          type_,
          description
        }

instance ToJSON ParameterStoreChange where
  toJSON change =
    object
      [ "operation" .= operation change,
        "name" .= name change,
        "type" .= type_ change,
        "description" .= description change
      ]

  toEncoding change =
    pairs $
      mconcat
        [ "operation" .= operation change,
          "name" .= name change,
          "type" .= type_ change,
          "description" .= description change
        ]

-- | AWS provides no schema for the @"operation"@ field, so we provide
-- a newtype wrapper and pattern synonyms which we think are complete,
-- based on AWS documentation.
newtype Operation = Operation Text deriving (Eq, Show, Generic)

pattern Create :: Operation
pattern Create = Operation "Create"

pattern Update :: Operation
pattern Update = Operation "Update"

pattern Delete :: Operation
pattern Delete = Operation "Delete"

pattern LabelParameterVersion :: Operation
pattern LabelParameterVersion = Operation "LabelParameterVersion"

{-# COMPLETE Create, Update, Delete, LabelParameterVersion #-}

instance FromJSON Operation where
  parseJSON = withText "Operation" $ pure . Operation

instance ToJSON Operation where
  toJSON (Operation op) = Aeson.String op
  toEncoding (Operation op) = text op

-- | AWS provides no schema for the @"type"@ field, but these are the
-- only three types of parameters you can create in Parameter Store.
data Type = String | StringList | SecureString
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON Type where
  parseJSON = withText "Type" $ \case
    "String" -> pure String
    "StringList" -> pure StringList
    "SecureString" -> pure SecureString
    t -> fail $ "Unrecognised type: " ++ show t

instance ToJSON Type where
  toJSON =
    Aeson.String . \case
      String -> "String"
      StringList -> "StringList"
      SecureString -> "SecureString"

  toEncoding =
    text . \case
      String -> "String"
      StringList -> "StringList"
      SecureString -> "SecureString"
