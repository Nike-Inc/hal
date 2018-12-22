{-|
Module      : AWS.Lambda.Internal
Description : Internal hal helper methods.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : unstable
-}

module AWS.Lambda.Internal (
  StaticContext(..),
  DynamicContext(..),
  mkContext
) where

import           AWS.Lambda.Context (ClientContext, CognitoIdentity,
                                     LambdaContext (LambdaContext))
import           Data.Time.Clock    (UTCTime)
import           GHC.Generics       (Generic)
import           System.Envy        (DefConfig (..), FromEnv, Option (..),
                                     fromEnv, gFromEnvCustom)

data StaticContext = StaticContext
  { functionName       :: String,
    functionVersion    :: String,
    functionMemorySize :: String,
    logGroupName       :: String,
    logStreamName      :: String
  } deriving (Show, Generic)

instance DefConfig StaticContext where
  defConfig = StaticContext "" "" "" "" ""

instance FromEnv StaticContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }

data DynamicContext = DynamicContext
  { awsRequestId       :: String,
    invokedFunctionArn :: String,
    xRayTraceId        :: String,
    deadline           :: UTCTime,
    clientContext      :: Maybe ClientContext,
    identity           :: Maybe CognitoIdentity
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
