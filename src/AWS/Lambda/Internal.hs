{-# LANGUAGE OverloadedStrings #-}

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
  getStaticContext,
  mkContext
) where

import           AWS.Lambda.Context (ClientContext, CognitoIdentity,
                                     LambdaContext (LambdaContext))
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text, pack)
import           Data.Time.Clock    (UTCTime)
import           GHC.Generics       (Generic)
import           System.Environment (getEnv)
import           Text.Read          (readMaybe)

data StaticContext = StaticContext
  { functionName       :: Text,
    functionVersion    :: Text,
    functionMemorySize :: Int,
    logGroupName       :: Text,
    logStreamName      :: Text
  } deriving (Show, Generic)

getStaticContext :: IO StaticContext
getStaticContext =
  StaticContext <$> (pack <$> getEnv "AWS_LAMBDA_FUNCTION_NAME") <*>
  (pack <$> getEnv "AWS_LAMBDA_FUNCTION_VERSION") <*>
  ((fromMaybe (error "AWS_LAMBDA_FUNCTION_MEMORY_SIZE was not an Int") . readMaybe) <$>
   getEnv "AWS_LAMBDA_FUNCTION_MEMORY_SIZE") <*>
  (pack <$> getEnv "AWS_LAMBDA_LOG_GROUP_NAME") <*>
  (pack <$> getEnv "AWS_LAMBDA_LOG_STREAM_NAME")

data DynamicContext = DynamicContext
  { awsRequestId       :: Text,
    invokedFunctionArn :: Text,
    xRayTraceId        :: Text,
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
