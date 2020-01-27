{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : AWS.Lambda.Events.ApiGateway.ProxyResponse
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events.ApiGateway.ProxyResponse
    ( module Network.HTTP.Types.Status
    , ProxyResponse(..)
    , ProxyBody(..)
    , textPlain
    , applicationJson
    , imageGif
    , imageJpeg
    ) where

import           Data.Aeson                (ToJSON, encode, object, toJSON,
                                            (.=))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as B64
import           Data.HashMap.Strict       (HashMap, insertWith)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Network.HTTP.Types.Status hiding (mkStatus,
                                            statusIsClientError,
                                            statusIsInformational,
                                            statusIsRedirection,
                                            statusIsServerError,
                                            statusIsSuccessful)

data ProxyBody = ProxyBody
    { contentType     :: T.Text
    , serialized      :: T.Text
    , isBase64Encoded :: Bool
    } deriving (Show)

data ProxyResponse = ProxyResponse
    { status  :: Status
    -- TODO: Case insensitive
    , headers :: HashMap T.Text T.Text
    , body    :: ProxyBody
    } deriving (Show)

genericBinary :: T.Text -> ByteString -> ProxyBody
genericBinary contentType x =
    ProxyBody contentType (TE.decodeUtf8 $ B64.encode x) True

-- Smart constructors for export
textPlain :: T.Text -> ProxyBody
textPlain x = ProxyBody "text/plain; charset=utf-8" x False

applicationJson :: ToJSON a => a -> ProxyBody
applicationJson x =
    ProxyBody
        "application/json; charset=utf-8"
        (TL.toStrict $ TLE.decodeUtf8 $ encode x)
        False

imageGif :: ByteString -> ProxyBody
imageGif = genericBinary "image/gif"

imageJpeg :: ByteString -> ProxyBody
imageJpeg = genericBinary "image/jpeg"

instance ToJSON ProxyResponse where
    toJSON (ProxyResponse status h (ProxyBody contentType body isBase64Encoded)) =
        object
            [ "statusCode" .= statusCode status
            , "headers" .=
              insertWith (\_ old -> old) "Content-Type" contentType h
            , "body" .= body
            , "isBase64Encoded" .= isBase64Encoded
            ]
