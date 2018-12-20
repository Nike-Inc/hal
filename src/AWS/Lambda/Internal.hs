module AWS.Lambda.Internal (
  StaticContext(..),
  DynamicContext(..),
  mkContext
) where

import           AWS.Lambda.Context       (ClientContext, CognitoIdentity, LambdaContext(LambdaContext))
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Map                 (Map)
import           GHC.Generics             (Generic)
import           System.Envy              (DefConfig (..), FromEnv, Option (..),
                                           fromEnv, gFromEnvCustom, Var(..))

data StaticContext = StaticContext
  { functionName             :: String,
    functionVersion          :: String,
    functionMemorySize       :: String,
    logGroupName             :: String,
    logStreamName            :: String
  } deriving (Show, Generic)

instance DefConfig StaticContext where
  defConfig = StaticContext "" "" "" "" ""

instance FromEnv StaticContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

data DynamicContext = DynamicContext
  { awsRequestId             :: String,
    invokedFunctionArn       :: String,
    xRayTraceId              :: String,
    deadlineMs               :: Double,
    clientContext            :: Maybe ClientContext,
    identity                 :: Maybe CognitoIdentity
  } deriving (Show)

mkContext :: StaticContext -> DynamicContext -> LambdaContext
mkContext static dynamic =
  LambdaContext
    0
    (functionName static)
    (functionVersion static)
    (functionMemorySize static)
    (logGroupName static)
    (logStreamName static)
    (awsRequestId dynamic)
    (invokedFunctionArn dynamic)
    (xRayTraceId dynamic)
    (deadlineMs dynamic)
    (clientContext dynamic)
    (identity dynamic)
