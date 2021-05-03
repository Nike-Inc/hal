{-|
Module      : AWS.Lambda.Events.ApiGateway.ProxyResponse
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable

This module enable exposes the required types for responding to API Gateway
Proxy Events.  Responses must return a status, body, and optionaly headers.
Multiple smart contructors and helpers are provided to help encapsulated
details like header case-insensitivity, multiple header copies, correct base64
encoding, and default content type.
-}
module AWS.Lambda.Events.ApiGateway.ProxyResponse
    ( ProxyResponse(..)
    , response
    , responseNoBody
    , addHeader
    , setHeader
    , ProxyBody(..)
    , textPlain
    , applicationJson
    , genericBinary
    , module Network.HTTP.Types.Status
    ) where

import           Data.Aeson                (ToJSON, encode, object, toJSON,
                                            (.=))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as B64
import           Data.CaseInsensitive      (CI, mk, original)
import           Data.HashMap.Strict       (HashMap, foldrWithKey, insert,
                                            insertWith)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Network.HTTP.Types.Status (Status (..), accepted202,
                                            badGateway502, badRequest400,
                                            conflict409, continue100,
                                            created201, expectationFailed417,
                                            forbidden403, found302,
                                            gatewayTimeout504, gone410,
                                            httpVersionNotSupported505,
                                            imATeapot418,
                                            internalServerError500,
                                            lengthRequired411,
                                            methodNotAllowed405,
                                            movedPermanently301,
                                            multipleChoices300,
                                            networkAuthenticationRequired511,
                                            noContent204, nonAuthoritative203,
                                            notAcceptable406, notFound404,
                                            notImplemented501, notModified304,
                                            ok200, partialContent206,
                                            paymentRequired402,
                                            permanentRedirect308,
                                            preconditionFailed412,
                                            preconditionRequired428,
                                            proxyAuthenticationRequired407,
                                            requestEntityTooLarge413,
                                            requestHeaderFieldsTooLarge431,
                                            requestTimeout408,
                                            requestURITooLong414,
                                            requestedRangeNotSatisfiable416,
                                            resetContent205, seeOther303,
                                            serviceUnavailable503, status100,
                                            status101, status200, status201,
                                            status202, status203, status204,
                                            status205, status206, status300,
                                            status301, status302, status303,
                                            status304, status305, status307,
                                            status308, status400, status401,
                                            status402, status403, status404,
                                            status405, status406, status407,
                                            status408, status409, status410,
                                            status411, status412, status413,
                                            status414, status415, status416,
                                            status417, status418, status422,
                                            status426, status428, status429,
                                            status431, status500, status501,
                                            status502, status503, status504,
                                            status505, status511,
                                            switchingProtocols101,
                                            temporaryRedirect307,
                                            tooManyRequests429, unauthorized401,
                                            unprocessableEntity422,
                                            unsupportedMediaType415,
                                            upgradeRequired426, useProxy305)

-- | Type that represents the body returned to an API Gateway when using HTTP
-- Lambda Proxy integration.  It is highly recommended that you do not use this
-- type directly, and instead use the smart constructors exposed such as
-- 'textPlain', 'applicationJson', and 'genericBinary'.  These make sure that
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
-- This type can be constructed explicity or via the smart constructor
-- `response`.  Headers can then be added incrementally with `addHeader` or
-- `setHeader`.  The smart constructor pattern is recommended because it avoids
-- some of the awkwardness of dealing with the multiValueHeaders field's type.
--
-- @
-- {-\# LANGUAGE NamedFieldPuns \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
--
-- module Main where
--
-- import AWS.Lambda.Runtime (pureRuntime)
-- import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest(..), NoAuthorizer)
-- import AWS.Lambda.Events.ApiGateway.ProxyResponse (ProxyResponse(..), textPlain, forbidden403, ok200, response)
--
-- myHandler :: ProxyRequest NoAuthorizer -> ProxyResponse
-- myHandler ProxyRequest { httpMethod = \"GET\", path = "/say_hello" } =
--     -- Smart Constructor and added header (recommended)
--     addHeader "My-Custom-Header" "Value" $
--       response ok200 $ textPlain \"Hello\"
-- myHandler _ =
--     -- Explicit Construction (not recommended)
--     ProxyResponse
--     {   status = forbidden403
--     ,   body = Just $ textPlain \"Forbidden\"
--     ,   multiValueHeaders =
--           fromList [(mk "My-Custom-Header", ["Other Value])]
--     }
--
-- main :: IO ()
-- main = pureRuntime myHandler
-- @
data ProxyResponse = ProxyResponse
    { status            :: Status
    , multiValueHeaders :: HashMap (CI T.Text) [T.Text]
    , body              :: Maybe ProxyBody
    } deriving (Show)

-- | Smart constructor for creating a @ProxyResponse@ from a status and a body
response :: Status -> ProxyBody -> ProxyResponse
response status body = ProxyResponse status mempty (Just body)

-- | Construct a @ProxyResponse@ with no body. Useful for redirects.
--
-- @since 0.5.0
responseNoBody :: Status -> ProxyResponse
responseNoBody status = ProxyResponse status mempty Nothing

-- | Add a header to the ProxyResponse.  If there was already a value for this
-- header, this one is __added__, meaning the response will include multiple
-- copies of this header (valid by the HTTP spec).  This does NOT replace any
-- previous headers or their values.
addHeader :: T.Text -> T.Text -> ProxyResponse -> ProxyResponse
addHeader header value (ProxyResponse s mvh b) =
  ProxyResponse s (insertWith (<>) (mk header) [value] mvh) b

-- | Set a header to the ProxyResponse.  If there were any previous values for
-- this header they are __all replaced__ by this new value.
setHeader :: T.Text -> T.Text -> ProxyResponse -> ProxyResponse
setHeader header value (ProxyResponse s mvh b) =
  ProxyResponse s (insert (mk header) [value] mvh) b

-- | Smart constructor for creating a ProxyBody with an arbitrary ByteString of
-- the chosen content type.  Use this smart constructor to avoid invalid JSON
-- representations of binary data.
--
-- From here it is easy to make more specific body constructors:
--
-- @
-- imageGif :: ByteString -> ProxyBody
-- imageGif = genericBinary "image/gif"
--
-- imageJpeg :: ByteString -> ProxyBody
-- imageJpeg = genericBinary "image/jpeg"
-- @
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

instance ToJSON ProxyResponse where
    toJSON (ProxyResponse status mvh mProxyBody) =
        let
            unCI = foldrWithKey (insert . original) mempty
            mvh' = case mProxyBody of
                Just (ProxyBody contentType _ _) ->
                    insertWith
                        (\_ old -> old)
                        ("Content-Type" :: T.Text)
                        [contentType]
                        (unCI mvh)
                Nothing -> unCI mvh
            bodyFields = case mProxyBody of
                Just (ProxyBody _ body isBase64Encoded) ->
                    [ "body" .= body
                    , "isBase64Encoded" .= isBase64Encoded
                    ]
                Nothing -> []

        in object $
               [ "statusCode" .= statusCode status
               , "multiValueHeaders" .= mvh'
               ] ++ bodyFields
