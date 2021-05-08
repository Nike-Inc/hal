module AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen where

import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen.Resource as Resource
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen.Parameters as Parameters
import           AWS.Lambda.Events.ApiGateway.ProxyRequest
import           Control.Applicative                        (liftA2)
import           Data.Aeson                                 (Value(..))
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.HashMap.Strict                        as H
import           Data.Scientific                            (fromFloatDigits)
import qualified Data.Vector                                as V
import qualified Gen.Header                                 as Header
import           Hedgehog                                   (Gen)
import qualified Hedgehog.Gen                               as Gen
import qualified Hedgehog.Range                             as Range
import           Network.HTTP.Types                         (renderStdMethod)
import           Data.Text                                  (Text)
import qualified Data.Text.Encoding                         as TE

proxyRequest :: Gen (ProxyRequest NoAuthorizer)
proxyRequest = do
  segments <- Resource.segments
  pathParameters <- Resource.vars segments
  let path = Resource.path pathParameters segments
      resource = Resource.resource segments
  multiValueHeaders <- Header.multiValueHeaders
  let headers = head <$> multiValueHeaders
  stageVariables <- fmap head <$> Parameters.multiParameters
  httpMethod <- method
  requestContext <- context path resource httpMethod
  multiValueQueryStringParameters <- Parameters.multiParameters
  let queryStringParameters = head <$> multiValueQueryStringParameters
  body <- BL.fromStrict <$> Gen.bytes (Range.exponential 1 1000)
  pure $ ProxyRequest
      { path = path
      , headers = headers
      , multiValueHeaders = multiValueHeaders
      , pathParameters = pathParameters
      , stageVariables = stageVariables
      , requestContext = requestContext
      , resource = resource
      , httpMethod = httpMethod
      , queryStringParameters = queryStringParameters
      , multiValueQueryStringParameters = multiValueQueryStringParameters
      , body = body
      }

method :: Gen Text
method = TE.decodeUtf8 . renderStdMethod <$> Gen.enumBounded

context :: Text -> Text -> Text -> Gen (RequestContext NoAuthorizer)
context path resourcePath httpMethod = do
  accountId <- Gen.text (Range.singleton 12) Gen.digit
  authorizer <- Gen.maybe authJson
  resourceId <- text
  stage <- text
  domainPrefix <- Gen.maybe $ Gen.text (Range.linear 1 15) Gen.alphaNum
  requestId <- text
  identity <- ident
  domainName <- Gen.maybe text
  extendedRequestId <- Gen.maybe text
  apiId <- text
  pure $ RequestContext
      { path = path
      , accountId = accountId
      , authorizer = authorizer
      , resourceId = resourceId
      , stage = stage
      , domainPrefix = domainPrefix
      , requestId = requestId
      , identity = identity
      , domainName = domainName
      , resourcePath = resourcePath
      , httpMethod = httpMethod
      , extendedRequestId = extendedRequestId
      , apiId = apiId
      }

ident :: Gen Identity
ident = Identity
  <$> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text
  <*> Gen.maybe text

text :: Gen Text
text = Gen.text (Range.linear 1 15) Gen.unicode

-- | Generate JSON for an authorizer blob.
--
-- __NOTE:__ We do not generate @null@s, because they kill
-- round-tripping: '.:?' deserializes both @null@ and missing keys to
-- @Nothing@.
authJson :: Gen Value
authJson = Gen.recursive Gen.choice
    [string, number, Bool <$> Gen.bool]
    [object, array]
    where
        object = Object . H.fromList <$> Gen.list (Range.linear 1 50) entry
        entry = liftA2 (,) text authJson
        array = Array . V.fromList <$> Gen.list (Range.linear 1 50) authJson
        string = String <$> Gen.text (Range.exponential 1 200) Gen.unicode
        number = Number . fromFloatDigits <$>
            Gen.double (Range.exponentialFloatFrom 0 (-10**23) (10**23))
