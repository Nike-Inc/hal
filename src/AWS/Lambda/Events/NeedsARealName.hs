{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : AWS.Lambda.Events.NeedsARealName
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events.NeedsARealName
    ( NeedsARealName(..)
    , needsARealName
    ) where

import           AWS.Lambda.Events.ApiGatewayProxyRequest (RequestContext)
import qualified AWS.Lambda.Events.ApiGatewayProxyRequest as Raw
import           Control.Monad                            ((>=>))
import           Data.Aeson                               (FromJSON, decode)
import           Data.Bifunctor                           (first)
import           Data.ByteString.Base64.Lazy              (encode)
import           Data.ByteString.Lazy                     (ByteString)
import           Data.CaseInsensitive                     (CI, mk)
import           Data.Map                                 (Map, fromList)
import           Data.Maybe                               (fromMaybe)
import           Data.Text.Lazy                           (Text)
import           Data.Text.Lazy.Encoding                  (encodeUtf8)
import           GHC.Generics                             (Generic (..))

data NeedsARealName a = NeedsARealName
    { path                            :: Text
    , headers                         :: Map (CI Text) Text
    , multiValueHeaders               :: Map (CI Text) [Text]
    , pathParameters                  :: Map Text Text
    , requestContext                  :: RequestContext
    , resource                        :: Text
    , httpMethod                      :: Text
    , queryStringParameters           :: Map Text Text
    , multiValueQueryStringParameters :: Map Text [Text]
    , body                            :: a
    } deriving (Generic)

needsARealName :: Raw.ApiGatewayProxyRequest -> NeedsARealName (Bool, ByteString)
needsARealName x =
    NeedsARealName
    { path =
        Raw.path x
    , headers =
        fromList $ fmap (first mk) $ fromMaybe mempty $ Raw.headers x
    , multiValueHeaders =
        fromList $ fmap (first mk) $ fromMaybe mempty $ Raw.multiValueHeaders x
    , pathParameters =
        fromList $ fromMaybe mempty $ Raw.pathParameters x
    , requestContext =
        Raw.requestContext x
    , resource =
        Raw.resource x
    , httpMethod =
        Raw.httpMethod x
    , queryStringParameters =
        fromList $ fromMaybe mempty $ Raw.queryStringParameters x
    , multiValueQueryStringParameters =
        fromList $ fromMaybe mempty $ Raw.multiValueQueryStringParameters x
    , body =
        (Raw.isBase64Encoded x, encodeUtf8 $ Raw.body x)
    }

expectJSON :: FromJSON a => NeedsARealName (Bool, ByteString) -> Maybe (NeedsARealName a)
expectJSON x@NeedsARealName { body } =
  case body of
    (True, _)  -> Nothing
    (False, a) -> (\b -> x { body = b }) <$> decode a

expectBase64 :: NeedsARealName (Bool, ByteString) -> Maybe (NeedsARealName ByteString)
expectBase64 x@NeedsARealName { body } =
  case body of
    (False, _) -> Nothing
    (True, b)  -> Just $ x { body = encode b }

-- TODO: Handler for functions that accept either?
-- Probably not a _great_ idea to do that, but probably should be supported
-- Likely better to make that and then define our expectXXX from it
needsARealNameJSON :: FromJSON a => Raw.ApiGatewayProxyRequest -> Maybe (NeedsARealName a)
needsARealNameJSON = expectJSON . needsARealName

needsARealNameBase64 :: Raw.ApiGatewayProxyRequest -> Maybe (NeedsARealName ByteString)
needsARealNameBase64 = expectBase64 . needsARealName
