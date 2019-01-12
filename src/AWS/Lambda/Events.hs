{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : AWS.Lambda.Events.Response
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events
    ( ApiGatewayProxyResponse(..)
    , ApiGatewayProxyRequest(..)
    ) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, FoldCase, mk, original)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic(..))
import Network.HTTP.Types
       (HeaderName, Method, Query, Status, statusCode)

data ApiGatewayProxyResponse a = ApiGatewayProxyResponse
    { resStatusCode :: Status
    -- TODO: name conflicts for being in the same module
    , resheaders :: Map Text Text
    , resbody :: a
    } deriving (Show, Generic)

data Identity = Identity
    { cognitoIdentityPoolId :: Text
    , accountId :: Text -- Same as before
    , cognitoIdentityId :: Text
    , caller :: Text -- what is this?
    , apiKey :: Text
    , sourceIp :: Text -- typeable?
    , cognitoAuthenticationType :: Text -- typeable? are this limited and well defined?
    , cognitoAuthenticationProvider :: Text -- typeable? are this limited and well defined?
    , userArn :: Text -- does amazonka type these?
    , userAgent :: Text -- can't imagine there's an effective way to type these
    , user :: Text -- no idea what this is
    }

data RequestContext = RequestContext
    { accountId :: Text -- This is well understood, does amazonka have a type for this?
    , resourceId :: Text -- what is this?
    , stage :: Text
    , requestId :: Text -- is there a UUID type?
    , identity :: Identity
    , resourcePath :: Text -- url path?
    , httpMethod :: Text --method?
    , apiId :: Text
    }

-- Some questions about how "accurately" we want to represent
-- the data given, and how much we want to abstract what we're given.
-- For example, why not just use 'multiValue' fields in the first place?
-- Or why use a disconnected bool when we could instead make
-- `body :: Either ByteStringBase64` a and drop `isBase64Encoded` altogether?
data ApiGatewayProxyRequest a = ApiGatewayProxyRequest
    { path :: Text
    , headers :: Map Text Text -- Might be nullable, but it seems like it _couldn't_ ever be empty
    , multiValueHeaders :: Map (CI Text) [Text] -- See "headers" 
    , pathParameters :: Map Text Text -- Can be null
    , requestContext :: RequestContext
    , resource :: Text
    , httpMethod :: Text
    , queryStringParameters :: Map Text Text -- Can be null
    , multiValueQueryStringParameters :: Map Text Text -- Can be null
    , isBase64Encoded :: Bool
    , body :: a
    } deriving (Show, Generic)

-- Hmmm... should we use this considering?
instance FromJSON (CI Text) where
    parseJSON = fmap mk . parseJSON

instance ToJSON a => ToJSON (CI a) where
    toJSON = toJSON . original

instance ToJSON Status where
    toJSON = toJSON . statusCode

--TODO, allow any FromJSON specialization
instance FromJSON (ApiGatewayProxyRequest String)

instance ToJSON (ApiGatewayProxyResponse String)
