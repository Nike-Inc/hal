{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

{-|
Module      : AWS.Lambda.Events.ApiGatewayProxyRequest
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events.ApiGatewayProxyRequest
    ( ApiGatewayProxyRequest(..)
    , RequestContext(..)
    , Identity(..)
    ) where

import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON, Value (Object),
                                              parseJSON, (.!=), (.:), (.:?))
import           Data.ByteString.Base64.Lazy (decodeLenient)
import           Data.ByteString.Lazy        (ByteString)
import           Data.CaseInsensitive        (CI, mk)
import           Data.HashMap.Strict         (HashMap, foldrWithKey, insert)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           GHC.Generics                (Generic (..))

data Identity = Identity
    { cognitoIdentityPoolId         :: Maybe Text
    , accountId                     :: Maybe Text
    , cognitoIdentityId             :: Maybe Text
    , caller                        :: Maybe Text
    , apiKey                        :: Maybe Text
    , sourceIp                      :: Text
    , accessKey                     :: Maybe Text
    , cognitoAuthenticationType     :: Maybe Text
    , cognitoAuthenticationProvider :: Maybe Text
    , userArn                       :: Maybe Text
    , apiKeyId                      :: Maybe Text
    , userAgent                     :: Maybe Text
    , user                          :: Maybe Text
    } deriving (Generic)

instance FromJSON Identity

data RequestContext = RequestContext
    { path              :: Text
    , accountId         :: Text
    , authorizer        :: HashMap Text Text
    , resourceId        :: Text
    , stage             :: Text
    , domainPrefix      :: Text
    , requestId         :: Text
    , identity          :: Identity
    , domainName        :: Text
    , resourcePath      :: Text
    , httpMethod        :: Text
    , extendedRequestId :: Text
    , apiId             :: Text
    }

instance FromJSON RequestContext where
  parseJSON (Object v) =
    RequestContext <$>
    v .: "path" <*>
    v .: "accountId" <*>
    v .:? "authorizer" .!= mempty <*>
    v .: "resourceId" <*>
    v .: "stage" <*>
    v .: "domainPrefix" <*>
    v .: "requestId" <*>
    v .: "identity" <*>
    v .: "domainName" <*>
    v .: "resourcePath" <*>
    v .: "httpMethod" <*>
    v .: "extendedRequestId" <*>
    v .: "apiId"
  parseJSON _ = mzero

-- TODO: Should also include websocket fields
data ApiGatewayProxyRequest = ApiGatewayProxyRequest
    { path                            :: Text
    , headers                         :: HashMap (CI Text) Text
    , multiValueHeaders               :: HashMap (CI Text) [Text]
    , pathParameters                  :: HashMap Text Text
    , stageVariables                  :: HashMap Text Text
    , requestContext                  :: RequestContext
    , resource                        :: Text
    , httpMethod                      :: Text
    , queryStringParameters           :: HashMap Text Text
    , multiValueQueryStringParameters :: HashMap Text [Text]
    , body                            :: ByteString
    } deriving (Generic)

toCIHashMap :: HashMap Text a -> HashMap (CI Text) a
toCIHashMap = foldrWithKey (insert . mk) mempty

toByteString :: Bool -> TL.Text -> ByteString
toByteString isBase64Encoded =
  if isBase64Encoded then
    decodeLenient . TLE.encodeUtf8
  else
    TLE.encodeUtf8

instance FromJSON ApiGatewayProxyRequest where
  parseJSON (Object v) =
    ApiGatewayProxyRequest <$>
    v .: "path" <*>
    (toCIHashMap <$> (v .:? "headers" .!= mempty)) <*>
    (toCIHashMap <$> (v .:? "multiValueHeaders" .!= mempty)) <*>
    v .:? "pathParameters" .!= mempty <*>
    v .:? "stageVariables" .!= mempty <*>
    v .: "requestContext" <*>
    v .: "resource" <*>
    v .: "httpMethod" <*>
    v .:? "queryStringParameters" .!= mempty <*>
    v .:? "multiValueQueryStringParameters" .!= mempty <*>
    (toByteString <$> v .: "isBase64Encoded" <*> v .: "body" .!= "")
  parseJSON _ = mzero
