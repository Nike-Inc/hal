module AWS.Lambda.Context (
  ClientApplication(..),
  ClientContext(..),
  CognitoIdentity(..),
  LambdaContext(..),
  HasLambdaContext(..),
  defConfig
) where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Map                 (Map)
import           GHC.Generics             (Generic)
import           System.Envy              (DefConfig (..), FromEnv, Option (..),
                                           fromEnv, gFromEnvCustom, Var(..))

data ClientApplication = ClientApplication
  { appTitle :: String,
    appVersionName :: String,
    appVersionCode :: String,
    appPackageName :: String
  } deriving (Show, Generic)

instance ToJSON ClientApplication
instance FromJSON ClientApplication

data ClientContext = ClientContext
  { client :: ClientApplication,
    custom :: Map String String,
    environment :: Map String String
  } deriving (Show, Generic)

instance ToJSON ClientContext
instance FromJSON ClientContext

data CognitoIdentity = CognitoIdentity
  { identityId :: String
  , identityPoolId :: String
  } deriving (Show, Generic)

instance ToJSON CognitoIdentity
instance FromJSON CognitoIdentity

data LambdaContext = LambdaContext
  { getRemainingTimeInMillis :: Double, -- TODO this is calculated by "us", Nathan and I talked about moving this into a function.
    functionName             :: String,
    functionVersion          :: String,
    functionMemorySize       :: String,
    logGroupName             :: String,
    logStreamName            :: String,
    -- The following context values come from headers rather than env vars.
    awsRequestId             :: String,
    invokedFunctionArn       :: String,
    xRayTraceId              :: String,
    deadlineMs               :: Double,
    clientContext            :: Maybe ClientContext,
    identity                 :: Maybe CognitoIdentity
  } deriving (Show, Generic)

class HasLambdaContext r where
  withContext :: (LambdaContext -> r -> r)

instance HasLambdaContext LambdaContext where
  withContext = const

instance DefConfig LambdaContext where
  defConfig = LambdaContext 0 "" "" "" "" "" "" "" "" 0 Nothing Nothing
