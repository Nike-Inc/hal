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
    awsRequestId             :: String,
    logGroupName             :: String,
    logStreamName            :: String,
    -- The following context values come from headers rather than env vars.
    invokedFunctionArn       :: String,
    xRayTraceId              :: String,
    deadlineMs               :: Double,
    clientContext            :: Maybe ClientContext,
    identity                 :: Maybe CognitoIdentity
  } deriving (Show, Generic)

-- TODO: Separate out static and dynamic context so this is not needed
instance Var ClientContext where
  toVar _ = ""
  fromVar _ = Just $ ClientContext (ClientApplication "" "" "" "") mempty mempty

-- TODO: Separate out static and dynamic context so this is not needed
instance Var CognitoIdentity where
  toVar _ = ""
  fromVar _ = Just $ CognitoIdentity "" ""

class HasLambdaContext r where
  withContext :: (LambdaContext -> r -> r)

instance HasLambdaContext LambdaContext where
  withContext = const

instance DefConfig LambdaContext where
  defConfig = LambdaContext 0 "" "" "" "" "" "" "" "" 0 Nothing Nothing

instance FromEnv LambdaContext where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0,
                    customPrefix = "AWS_LAMBDA"
          }
