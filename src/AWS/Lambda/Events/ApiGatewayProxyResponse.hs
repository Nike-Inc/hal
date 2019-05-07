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
    , LambdaSerializable(..)
    , ApplicationJson
    , applicationJson
    , Binary
    , imageGif
    , imageJpeg
    ) where

import           Data.Aeson              (ToJSON, encode, object, toJSON, (.=))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base64  as B64
import           Data.HashMap.Strict     (HashMap, insertWith)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

data ApiGatewayProxyResponse a = ApiGatewayProxyResponse
    { statusCode :: Int
    , headers    :: HashMap T.Text T.Text
    , body       :: a
    } deriving Show

class LambdaSerializable a where
  serialize :: a -> T.Text
  contentType :: a -> T.Text

instance LambdaSerializable T.Text where
  serialize = id
  contentType _ = "text/plain; charset=utf8"

instance LambdaSerializable TL.Text where
  serialize = TL.toStrict
  contentType _ = "text/plain; charset=utf8"

instance LambdaSerializable String where
  serialize = T.pack
  contentType _ = "text/plain; charset=utf8"

newtype ApplicationJson a = ApplicationJson a

instance ToJSON a => LambdaSerializable (ApplicationJson a) where
  serialize (ApplicationJson j) = TL.toStrict $ TLE.decodeUtf8 $ encode j
  contentType _ = "application/json; charset=utf8"

data Binary
  = ImageGif_ ByteString
  | ImageJpeg_ ByteString

instance LambdaSerializable Binary where
  serialize (ImageGif_ b)  = TE.decodeUtf8 $ B64.encode b
  serialize (ImageJpeg_ b) = TE.decodeUtf8 $ B64.encode b
  contentType (ImageGif_ _)  = "image/gif"
  contentType (ImageJpeg_ _) = "image/jpeg"

-- Smart constructors for export

applicationJson :: a -> ApplicationJson a
applicationJson = ApplicationJson

imageGif :: ByteString -> Binary
imageGif = ImageGif_

imageJpeg :: ByteString -> Binary
imageJpeg = ImageJpeg_

instance LambdaSerializable a => ToJSON (ApiGatewayProxyResponse a) where
  toJSON (ApiGatewayProxyResponse sc h b) =
    object
      [ "statusCode" .= sc
      , "headers" .= insertWith (\_ old -> old) "Content-Type" (contentType b) h
      , "body" .= serialize b
      ]
