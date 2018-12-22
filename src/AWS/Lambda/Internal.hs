{-# LANGUAGE OverloadedStrings #-}
module AWS.Lambda.Internal (
  StaticContext(..),
  DynamicContext(..),
  mkContext
) where

import           AWS.Lambda.Context       (ClientContext, CognitoIdentity, LambdaContext(LambdaContext))
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Map                 (Map)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Data.Time.Clock          (UTCTime)
import           System.Envy              (DefConfig (..), FromEnv, Option (..),
                                           fromEnv, gFromEnvCustom, Var(..))

data StaticContext = StaticContext
  { functionName             :: Text,
    functionVersion          :: Text,
    functionMemorySize       :: Int,
    logGroupName             :: Text,
    logStreamName            :: Text
  } deriving (Show, Generic)

instance DefConfig StaticContext where
  defConfig = StaticContext "" "" 0 "" ""

instance FromEnv StaticContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

data DynamicContext = DynamicContext
  { awsRequestId             :: Text,
    invokedFunctionArn       :: Text,
    xRayTraceId              :: Text,
    deadline                 :: UTCTime,
    clientContext            :: Maybe ClientContext,
    identity                 :: Maybe CognitoIdentity
  } deriving (Show)

mkContext :: StaticContext -> DynamicContext -> LambdaContext
mkContext static dynamic =
  LambdaContext
    (functionName static)
    (functionVersion static)
    (functionMemorySize static)
    (logGroupName static)
    (logStreamName static)
    (awsRequestId dynamic)
    (invokedFunctionArn dynamic)
    (xRayTraceId dynamic)
    (deadline dynamic)
    (clientContext dynamic)
    (identity dynamic)
