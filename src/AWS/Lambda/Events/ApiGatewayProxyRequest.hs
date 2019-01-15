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
    ) where

import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic (..))

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
    , authorizer   :: Map Text Text
    , resourceId   :: Text
    , stage        :: Text
    , requestId    :: Text
    , identity     :: Identity
    , resourcePath :: Text
    , httpMethod   :: Text
    , apiId        :: Text
    } deriving (Generic)

instance FromJSON RequestContext

data ApiGatewayProxyRequest a = ApiGatewayProxyRequest
    { path                            :: Text
    , headers                         :: Maybe [(Text, Text)]
    , multiValueHeaders               :: Maybe [(Text, [Text])]
    , pathParameters                  :: Maybe [(Text, Text)]
    , requestContext                  :: RequestContext
    , resource                        :: Text
    , httpMethod                      :: Text
    , queryStringParameters           :: Maybe [(Text, Text)]
    , multiValueQueryStringParameters :: Maybe [(Text, [Text])]
    , isBase64Encoded                 :: Bool
    , body                            :: a
    } deriving (Generic)

instance FromJSON a => FromJSON (ApiGatewayProxyRequest a)
