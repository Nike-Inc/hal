{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : AWS.Lambda.Events.ApiGatewayProxyResponse
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events.ApiGatewayProxyResponse
    ( ApiGatewayProxyResponse(..)
    , ApiGatewayProxyBody(..)
    , textPlain
    , applicationJson
    , imageGif
    , imageJpeg
    ) where

import           Data.Aeson                (ToJSON, encode, object, toJSON, (.=))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as B64
import           Data.HashMap.Strict       (HashMap, insertWith)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Network.HTTP.Types.Status (Status(..))

data ApiGatewayProxyBody = ApiGatewayProxyBody
  { contentType :: T.Text
  , serialized :: T.Text
  , isBase64Encoded :: Bool
  } deriving Show

data ApiGatewayProxyResponse = ApiGatewayProxyResponse
    { status  :: Status
    , headers :: HashMap T.Text T.Text
    , body    :: ApiGatewayProxyBody
    } deriving Show

genericBinary :: T.Text -> ByteString -> ApiGatewayProxyBody
genericBinary contentType x = ApiGatewayProxyBody contentType (TE.decodeUtf8 $ B64.encode x) True

-- Smart constructors for export

textPlain :: T.Text -> ApiGatewayProxyBody
textPlain x = ApiGatewayProxyBody "text/plain; charset=utf-8" x False

applicationJson :: ToJSON a => a -> ApiGatewayProxyBody
applicationJson x = ApiGatewayProxyBody
  "application/json; charset=utf-8"
  (TL.toStrict $ TLE.decodeUtf8 $ encode x)
  False

imageGif :: ByteString -> ApiGatewayProxyBody
imageGif = genericBinary "image/gif"

imageJpeg :: ByteString -> ApiGatewayProxyBody
imageJpeg = genericBinary "image/jpeg"

instance ToJSON ApiGatewayProxyResponse where
  toJSON (ApiGatewayProxyResponse status h (ApiGatewayProxyBody contentType body isBase64Encoded)) =
    object
      [ "statusCode" .= statusCode status
      , "headers" .= insertWith (\_ old -> old) "Content-Type" contentType h
      , "body" .= body
      , "isBase64Encoded" .= isBase64Encoded
      ]
