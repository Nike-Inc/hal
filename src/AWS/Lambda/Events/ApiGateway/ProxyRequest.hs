{-# LANGUAGE CPP               #-}
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

import           Data.Aeson                  (FromJSON(..), ToJSON(..),
                                              Value(..), object, withObject,
                                              (.=), (.:), (.:?))
import           Data.ByteString.Base64.Lazy (decodeLenient, encode)
import           Data.ByteString.Lazy        (ByteString)
import           Data.CaseInsensitive        (CI, FoldCase, mk, original)
import           Data.Foldable               (fold)
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap, foldrWithKey, insert)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLE
import           Data.Void                   (Void)
import           GHC.Generics                (Generic (..))

-- This function is available in Data.Functor as of base 4.11, but we define it
-- here for compatibility.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) x f = f <$> x

infixl 1 <&>

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
    } deriving (Eq, Generic, Show)

instance FromJSON Identity
instance ToJSON Identity

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
    } deriving (Eq, Generic, Show)

instance FromJSON a => FromJSON (RequestContext a) where
    parseJSON = withObject "ProxyRequest" $ \v ->
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

-- | @since 0.4.8
instance ToJSON a => ToJSON (RequestContext a) where
    toJSON r = object $ catMaybes $
        let
            RequestContext { path = p, accountId = a, httpMethod = h } = r
        in
            [ Just $ "path" .= p
            , Just $ "accountId" .= a
            , ("authorizer" .=) <$> authorizer r
            , Just $ "resourceId" .= resourceId r
            , Just $ "stage" .= stage r
            , ("domainPrefix" .=) <$> domainPrefix r
            , Just $ "requestId" .= requestId r
            , Just $ "identity" .= identity r
            , ("domainName" .=) <$> domainName r
            , Just $ "resourcePath" .= resourcePath r
            , Just $ "httpMethod" .= h
            , ("extendedRequestId" .=) <$> extendedRequestId r
            , Just $ "apiId" .= apiId r
            ]

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
    } deriving (Eq, Generic, Show)

toCIHashMap ::
#if !MIN_VERSION_hashable(1,4,0)
    Eq k =>
#endif
    FoldCase k =>
    Hashable k =>
    HashMap k a ->
    HashMap (CI k) a
toCIHashMap = foldrWithKey (insert . mk) mempty

fromCIHashMap ::
#if !MIN_VERSION_hashable(1,4,0)
    Eq k =>
#endif
    Hashable k =>
    HashMap (CI k) a ->
    HashMap k a
fromCIHashMap = foldrWithKey (insert . original) mempty

toByteString :: Bool -> TL.Text -> ByteString
toByteString isBase64Encoded =
    if isBase64Encoded
        then decodeLenient . TLE.encodeUtf8
        else TLE.encodeUtf8

toMaybe :: Bool -> a -> Maybe a
toMaybe b a = if b then Just a else Nothing

-- | For ignoring API Gateway custom authorizer values
type NoAuthorizer = Value

-- | For ensuring that there were no API Gateway custom authorizer values (this
-- is not likely to be useful, you probably want 'NoAuthorizer')
type StrictlyNoAuthorizer = Void

instance FromJSON a => FromJSON (ProxyRequest a) where
    parseJSON = withObject "ProxyRequest" $ \v ->
        ProxyRequest <$> v .: "path" <*>
        (v .:? "headers" <&> toCIHashMap . fold) <*>
        (v .:? "multiValueHeaders" <&> toCIHashMap . fold) <*>
        (v .:? "pathParameters" <&> fold) <*>
        (v .:? "stageVariables" <&> fold) <*>
        v .: "requestContext" <*>
        v .: "resource" <*>
        v .: "httpMethod" <*>
        (v .:? "queryStringParameters" <&> fold) <*>
        (v .:? "multiValueQueryStringParameters" <&> fold) <*>
        (toByteString <$> v .: "isBase64Encoded" <*> (v .:? "body" <&> fold))

-- | @since 0.4.8
instance ToJSON a => ToJSON (ProxyRequest a) where
    toJSON r = object $ catMaybes $
        let
            ProxyRequest { path = p, httpMethod = h } = r
        in
            [ Just $ "path" .= p
            , toMaybe (not . null $ headers r) $
                  "headers" .= fromCIHashMap (headers r)
            , toMaybe (not . null $ multiValueHeaders r) $
                  "multiValueHeaders" .= fromCIHashMap (multiValueHeaders r)
            , toMaybe (not . null $ pathParameters r) $
                  "pathParameters" .= pathParameters r
            , toMaybe (not . null $ stageVariables r) $
                  "stageVariables" .= stageVariables r
            , Just $ "requestContext" .= requestContext r
            , Just $ "resource" .= resource r
            , Just $ "httpMethod" .= h
            , toMaybe (not . null $ queryStringParameters r) $
                  "queryStringParameters" .= queryStringParameters r
            , toMaybe (not . null $ multiValueQueryStringParameters r) $
                  "multiValueQueryStringParameters" .=
                      multiValueQueryStringParameters r
            , Just $ "isBase64Encoded" .= True
            , Just $ "body" .= TLE.decodeUtf8 (encode (body r))
            ]
