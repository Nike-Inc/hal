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
    , RequestContext
    , Identity
    ) where

import           Data.Aeson           (FromJSON, Value (Object), parseJSON,
                                       (.!=), (.:), (.:?))
import           Data.Aeson.Types     (Parser)
import           Data.CaseInsensitive (CI, mk)
import           Data.HashMap.Strict  (HashMap, foldrWithKey, insert, lookup)
import           Data.Text.Lazy       (Text)
import           GHC.Generics         (Generic (..))

data Identity = Identity
    { cognitoIdentityPoolId         :: Maybe Text
    , accountId                     :: Maybe Text
    , cognitoIdentityId             :: Maybe Text
    , caller                        :: Maybe Text
    , apiKey                        :: Maybe Text
    , sourceIp                      :: Text
    , cognitoAuthenticationType     :: Maybe Text
    , cognitoAuthenticationProvider :: Maybe Text
    , userArn                       :: Maybe Text
    , userAgent                     :: Maybe Text
    , user                          :: Maybe Text
    } deriving (Generic)

instance FromJSON Identity

data RequestContext = RequestContext
    { accountId    :: Text
    , authorizer   :: HashMap Text Text
    , resourceId   :: Text
    , stage        :: Text
    , requestId    :: Text
    , identity     :: Identity
    , resourcePath :: Text
    , httpMethod   :: Text
    , apiId        :: Text
    }

instance FromJSON RequestContext where
  parseJSON (Object v) =
    RequestContext <$>
    v .: "accountId" <*>
    v .:? "authorizer" .!= mempty <*>
    v .: "resourceId" <*>
    v .: "stage" <*>
    v .: "requestId" <*>
    v .: "identity" <*>
    v .: "resourcePath" <*>
    v .: "httpMethod" <*>
    v .: "apiId"

-- TODO: Should also include websocket fields
data ApiGatewayProxyRequest a = ApiGatewayProxyRequest
    { path                            :: Text
    , headers                         :: HashMap (CI Text) Text
    , multiValueHeaders               :: HashMap (CI Text) [Text]
    , pathParameters                  :: HashMap Text Text
    , requestContext                  :: RequestContext
    , resource                        :: Text
    , httpMethod                      :: Text
    , queryStringParameters           :: HashMap Text Text
    , multiValueQueryStringParameters :: HashMap Text [Text]
    , isBase64Encoded                 :: Bool
    , body                            :: a
    } deriving (Generic)

toCIHashMap :: HashMap Text a -> HashMap (CI Text) a
toCIHashMap = foldrWithKey (insert . mk) mempty

instance FromJSON (ApiGatewayProxyRequest Text) where
  parseJSON (Object v) =
    ApiGatewayProxyRequest <$>
    v .: "path" <*>
    (toCIHashMap <$> (v .:? "headers" .!= mempty)) <*>
    (toCIHashMap <$> (v .:? "multiValueHeaders" .!= mempty)) <*>
    v .:? "pathParameters" .!= mempty <*>
    v .: "requestContext" <*>
    v .: "resource" <*>
    v .: "httpMethod" <*>
    v .:? "queryStringParameters" .!= mempty <*>
    v .:? "multiValueQueryStringParameters" .!= mempty <*>
    v .: "isBase64Encoded" <*>
    v .: "body"

instance Functor ApiGatewayProxyRequest where
  fmap f x = x { body = f $ body x }
