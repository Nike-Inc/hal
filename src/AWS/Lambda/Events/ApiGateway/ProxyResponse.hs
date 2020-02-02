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
import           Data.CaseInsensitive      (CI, original)
import           Data.HashMap.Strict       (HashMap, foldrWithKey, insert,
                                            insertWith)
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

-- | Type that represents the body returned to an API Gateway when using HTTP
-- Lambda Proxy integration.  It is highly recommended that you do not use this
-- type directly, and instead use the smart constructors exposed such as
-- 'textPlain', 'applicationJSON', and 'genericBinary'.  These make sure that
-- the base64 encodings work transparently.
data ProxyBody = ProxyBody
    { contentType     :: T.Text
    , serialized      :: T.Text
    , isBase64Encoded :: Bool
    } deriving (Show)

-- | A response returned to an API Gateway when using the HTTP Lambda Proxy
-- integration.  ContentType will be set based on the ProxyBody (recommended)
-- if a value is not present in the headers field.
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
data ProxyResponse = ProxyResponse
    { status            :: Status
    , headers           :: HashMap (CI T.Text) T.Text
    , multiValueHeaders :: HashMap (CI T.Text) [T.Text]
    , body              :: ProxyBody
    } deriving (Show)

-- | Smart constructor for creating a ProxyBody with an arbitrary ByteString of
-- the chosen content type.
genericBinary :: T.Text -> ByteString -> ProxyBody
genericBinary contentType x =
    ProxyBody contentType (TE.decodeUtf8 $ B64.encode x) True

-- | Smart constructor for creating a simple body of text.
textPlain :: T.Text -> ProxyBody
textPlain x = ProxyBody "text/plain; charset=utf-8" x False

-- | Smart constructor for creating a simple body of JSON.
applicationJson :: ToJSON a => a -> ProxyBody
applicationJson x =
    ProxyBody
        "application/json; charset=utf-8"
        (TL.toStrict $ TLE.decodeUtf8 $ encode x)
        False

-- | Smart constructor for creating a simple body of a GIF (that has already
-- been converted to a ByteString).
imageGif :: ByteString -> ProxyBody
imageGif = genericBinary "image/gif"

-- | Smart constructor for creating a simple body of a JPEG (that has already
-- been converted to a ByteString).
imageJpeg :: ByteString -> ProxyBody
imageJpeg = genericBinary "image/jpeg"

instance ToJSON ProxyResponse where
    toJSON (ProxyResponse status h mvh (ProxyBody contentType body isBase64Encoded)) =
        let unCI = foldrWithKey (insert . original) mempty
        in object
               [ "statusCode" .= statusCode status
               , "headers" .=
                 insertWith
                     (\_ old -> old)
                     ("Content-Type" :: T.Text)
                     contentType
                     (unCI h)
               , "multiValueHeaders" .= unCI mvh
               , "body" .= body
               , "isBase64Encoded" .= isBase64Encoded
               ]
