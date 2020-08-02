{-|
Module      : AWS.Lambda.Events.ApiGateway.ProxyRequest
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable

This module exposes types used to model incoming __proxy__ requests from AWS
API Gateway.  These types are a light pass over the incoming JSON
representation.
-}
module AWS.Lambda.Events.ApiGateway.ProxyRequest
    ( ProxyRequest(..)
    , RequestContext(..)
    , Identity(..)
    , NoAuthorizer
    , StrictlyNoAuthorizer
    ) where

import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON, Value (Object),
                                              parseJSON, (.!=), (.:), (.:?))
import           Data.ByteString.Base64.Lazy (decodeLenient)
import           Data.ByteString.Lazy        (ByteString)
import           Data.CaseInsensitive        (CI, mk)
import           Data.HashMap.Strict         (HashMap, foldrWithKey, insert)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLE
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

data RequestContext a = RequestContext
    { path              :: Text
    , accountId         :: Text
    , authorizer        :: Maybe a
    , resourceId        :: Text
    , stage             :: Text
    , domainPrefix      :: Maybe Text
    , requestId         :: Text
    , identity          :: Identity
    , domainName        :: Maybe Text
    , resourcePath      :: Text
    , httpMethod        :: Text
    , extendedRequestId :: Maybe Text
    , apiId             :: Text
    }

instance FromJSON a => FromJSON (RequestContext a) where
    parseJSON (Object v) =
        RequestContext <$> v .: "path" <*> v .: "accountId" <*>
        v .:? "authorizer" <*>
        v .: "resourceId" <*>
        v .: "stage" <*>
        v .:? "domainPrefix" <*>
        v .: "requestId" <*>
        v .: "identity" <*>
        v .:? "domainName" <*>
        v .: "resourcePath" <*>
        v .: "httpMethod" <*>
        v .:? "extendedRequestId" <*>
        v .: "apiId"
    parseJSON _ = mzero

-- TODO: Should also include websocket fields
-- | This type is for representing events that come from API Gateway via the
-- Lambda Proxy integration (forwarding HTTP data directly, rather than a
-- custom integration).  It will automatically decode the event that comes in.
--
-- The 'ProxyRequest' notably has one parameter for the type of information
-- returned by the API Gateway's custom authorizer (if applicable).  This type
-- must also implement FromJSON so that it can be decoded.  If you do not
-- expect this data to be populated we recommended using the 'NoAuthorizer'
-- type exported from this module (which is just an alias for 'Value').  If
-- there _must not_ be authorizer populated (this is unlikely) then use the
-- 'StrictlyNoAuthorizer' type.
--
-- @
--     {-\# LANGUAGE NamedFieldPuns \#-}
--     {-\# LANGUAGE DuplicateRecordFields \#-}
--
--     module Main where
--
--     import AWS.Lambda.Runtime (pureRuntime)
--     import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest(..), NoAuthorizer)
--     import AWS.Lambda.Events.ApiGateway.ProxyResponse (ProxyResponse(..), textPlain, forbidden403, ok200)
--
--     myHandler :: ProxyRequest NoAuthorizer -> ProxyResponse
--     myHandler ProxyRequest { httpMethod = \"GET\", path = "/say_hello" } =
--         ProxyResponse
--         {   status = ok200
--         ,   body = textPlain \"Hello\"
--         ,   headers = mempty
--         ,   multiValueHeaders = mempty
--         }
--     myHandler _ =
--         ProxyResponse
--         {   status = forbidden403
--         ,   body = textPlain \"Forbidden\"
--         ,   headers = mempty
--         ,   multiValueHeaders = mempty
--         }
--
--     main :: IO ()
--     main = pureRuntime myHandler
-- @
data ProxyRequest a = ProxyRequest
    { path                            :: Text
    , headers                         :: HashMap (CI Text) Text
    , multiValueHeaders               :: HashMap (CI Text) [Text]
    , pathParameters                  :: HashMap Text Text
    , stageVariables                  :: HashMap Text Text
    , requestContext                  :: RequestContext a
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
    if isBase64Encoded
        then decodeLenient . TLE.encodeUtf8
        else TLE.encodeUtf8

-- | For ignoring API Gateway custom authorizer values
type NoAuthorizer = Value

-- | For ensuring that there were no API Gateway custom authorizer values (this
-- is not likely to be useful, you probably want 'NoAuthorizer')
type StrictlyNoAuthorizer = ()

instance FromJSON a => FromJSON (ProxyRequest a) where
    parseJSON (Object v) =
        ProxyRequest <$> v .: "path" <*>
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
