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
    { cognitoIdentityPoolId         :: Text
    , accountId                     :: Text -- Same as before
    , cognitoIdentityId             :: Text
    , caller                        :: Text -- what is this?
    , apiKey                        :: Text
    , sourceIp                      :: Text -- typeable?
    , cognitoAuthenticationType     :: Text -- typeable? are this limited and well defined?
    , cognitoAuthenticationProvider :: Text -- typeable? are this limited and well defined?
    , userArn                       :: Text -- does amazonka type these?
    , userAgent                     :: Text -- can't imagine there's an effective way to type these
    , user                          :: Text -- no idea what this is
    } deriving (Generic)

instance FromJSON Identity

data RequestContext = RequestContext
    { accountId    :: Text -- This is well understood, does amazonka have a type for this?
    , resourceId   :: Text -- what is this?
    , stage        :: Text
    , requestId    :: Text -- is there a UUID type?
    , identity     :: Identity
    , resourcePath :: Text -- url path?
    , httpMethod   :: Text --method?
    , apiId        :: Text
    } deriving (Generic)

instance FromJSON RequestContext

data ApiGatewayProxyRequest a = ApiGatewayProxyRequest
    { path                            :: Text
    , headers                         :: [(Text, Text)] -- Might be nullable, but it seems like it _couldn't_ ever be empty
    , multiValueHeaders               :: [(Text, [Text])] -- See "headers"
    , pathParameters                  :: [(Text, Text)] -- Can be null
    , requestContext                  :: RequestContext
    , resource                        :: Text
    , httpMethod                      :: Text
    , queryStringParameters           :: [(Text, Text)] -- Can be null
    , multiValueQueryStringParameters :: [(Text, Text)] -- Can be null
    , isBase64Encoded                 :: Bool
    , body                            :: a
    } deriving (Generic)

instance FromJSON a => FromJSON (ApiGatewayProxyRequest a)
