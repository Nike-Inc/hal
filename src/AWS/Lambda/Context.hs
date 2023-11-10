{-# LANGUAGE NamedFieldPuns    #-}


{-|
Module      : AWS.Lambda.Context
Description : AWS Lambda Context classes and related methods.
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Context (
  ClientApplication(..),
  ClientContext(..),
  CognitoIdentity(..),
  LambdaContext(..),
  getRemainingTime,
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           Data.Time.Clock        (DiffTime, UTCTime, diffUTCTime,
                                         getCurrentTime)
import           GHC.Generics           (Generic)

data ClientApplication = ClientApplication
  { appTitle       :: Text,
    appVersionName :: Text,
    appVersionCode :: Text,
    appPackageName :: Text
  } deriving (Show, Generic, Eq)

instance ToJSON ClientApplication
instance FromJSON ClientApplication

data ClientContext = ClientContext
  { client      :: ClientApplication,
    custom      :: Map Text Text,
    environment :: Map Text Text
  } deriving (Show, Generic, Eq)

instance ToJSON ClientContext
instance FromJSON ClientContext

data CognitoIdentity = CognitoIdentity
  { identityId     :: Text
  , identityPoolId :: Text
  } deriving (Show, Generic, Eq)

instance ToJSON CognitoIdentity
instance FromJSON CognitoIdentity

getRemainingTime :: MonadIO m => LambdaContext -> m DiffTime
getRemainingTime LambdaContext { deadline } =
  liftIO $ fmap (realToFrac . diffUTCTime deadline) getCurrentTime

data LambdaContext = LambdaContext
  { functionName       :: Text,
    functionVersion    :: Text,
    functionMemorySize :: Int,
    logGroupName       :: Text,
    logStreamName      :: Text,
    -- The following context values come from headers rather than env vars.
    awsRequestId       :: Text,
    invokedFunctionArn :: Text,
    xRayTraceId        :: Text,
    deadline           :: UTCTime,
    clientContext      :: Maybe ClientContext,
    identity           :: Maybe CognitoIdentity
  } deriving (Show, Generic, Eq)
