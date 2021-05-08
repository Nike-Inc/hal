{-# LANGUAGE TupleSections #-}

module AWS.Lambda.Events.ApiGateway.ProxyResponse.Gen where

import           AWS.Lambda.Events.ApiGateway.ProxyResponse
import qualified Data.ByteString.Base64                     as B64
import           Data.CaseInsensitive                       (CI)
import qualified Data.CaseInsensitive                       as CI
import           Data.HashMap.Strict                        (HashMap)
import qualified Data.Set                                   as S
import           Data.Text                                  (Text)
import qualified Data.Text.Encoding                         as TE
import qualified Gen.Header                                 as Header
import           Hedgehog
import qualified Hedgehog.Gen                               as Gen
import qualified Hedgehog.Range                             as Range
import           Network.HTTP.Types                         (hContentType)

proxyResponse :: Gen ProxyResponse
proxyResponse = ProxyResponse
    <$> responseStatus
    <*> responseHeaders
    <*> responseBody

responseStatus :: Gen Status
responseStatus = Gen.element
    [ continue100
    , switchingProtocols101
    , ok200
    , created201
    , accepted202
    , nonAuthoritative203
    , noContent204
    , resetContent205
    , partialContent206
    , multipleChoices300
    , movedPermanently301
    , found302
    , seeOther303
    , notModified304
    , useProxy305
    , temporaryRedirect307
    , permanentRedirect308
    , badRequest400
    , unauthorized401
    , paymentRequired402
    , forbidden403
    , notFound404
    , methodNotAllowed405
    , notAcceptable406
    , proxyAuthenticationRequired407
    , requestTimeout408
    , conflict409
    , gone410
    , lengthRequired411
    , preconditionFailed412
    , requestEntityTooLarge413
    , requestURITooLong414
    , unsupportedMediaType415
    , requestedRangeNotSatisfiable416
    , expectationFailed417
    , imATeapot418
    , unprocessableEntity422
    , upgradeRequired426
    , preconditionRequired428
    , tooManyRequests429
    , requestHeaderFieldsTooLarge431
    , internalServerError500
    , notImplemented501
    , badGateway502
    , serviceUnavailable503
    , gatewayTimeout504
    , httpVersionNotSupported505
    , networkAuthenticationRequired511
    ]

-- | Do not generate @Content-Type@ headers, as they will break
-- round-tripping: the 'ToJSON' instance for 'ProxyResponse' drops the
-- 'contentType' of the 'ProxyBody' if such a header is present.
responseHeaders :: Gen (HashMap (CI Text) [Text])
responseHeaders = Header.multiValueHeadersExcept
    . S.singleton
    $ CI.map TE.decodeUtf8 hContentType

responseBody :: Gen ProxyBody
responseBody = do
  contentType <- Gen.text (Range.linear 1 20) Gen.latin1
  (serialized, isBase64Encoded) <- Gen.choice
      [ (, False) <$> Gen.text (Range.exponential 1 2000) Gen.unicode
      , (, True) . TE.decodeUtf8 . B64.encode <$>
            Gen.bytes (Range.exponential 1 2000)
      ]
  pure $ ProxyBody contentType serialized isBase64Encoded
