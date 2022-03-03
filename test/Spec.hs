import           AWS.Lambda.Context                 (ClientApplication (..),
                                                     ClientContext (..),
                                                     CognitoIdentity (..),
                                                     LambdaContext (..))
import qualified Gen.Header                         as Header
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Spec as ProxyRequest
import qualified AWS.Lambda.Events.ApiGateway.ProxyResponse.Spec as ProxyResponse

import qualified AWS.Lambda.Events.EventBridge.Spec as EventBridge
import qualified AWS.Lambda.Events.Kafka.Spec       as Kafka
import           AWS.Lambda.Internal                (StaticContext (..))
import           AWS.Lambda.RuntimeClient.Internal  (eventResponseToNextData)
import           Data.Aeson                         (Value (Null))
import qualified Data.CaseInsensitive               as CI
import           Data.Map                           (singleton)
import           Data.Semigroup                     ((<>))
import           Data.Time.Clock.POSIX              (posixSecondsToUTCTime)
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import           Network.HTTP.Client.Internal       (Response (..))
import           Network.HTTP.Types                 (Header)
import           Test.Hspec                         (describe, it, shouldBe,
                                                     shouldStartWith, specify)
import           Test.Hspec.Hedgehog                (hedgehog)
import           Test.Hspec.Runner                  (hspec)

main :: IO ()
main =
  hspec $ do
    describe "Events" $ do
      describe "ApiGateway" $ do
        describe "ProxyRequest" ProxyRequest.spec
        describe "ProxyResponse" ProxyResponse.spec
      describe "EventBridge" EventBridge.spec
      describe "Kafka" Kafka.spec

    describe "Event Response Data" $ do
      let staticContext =
            StaticContext
              { functionName = "name"
              , functionVersion = "version"
              , functionMemorySize = 128
              , logGroupName = "logGroupName"
              , logStreamName = "logStreamName"
              }
      let basicValidHeaders =
            [ ("Lambda-Runtime-Aws-Request-Id", "abc")
            , ("Lambda-Runtime-Trace-Id", "123")
            , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
            , ("Lambda-Runtime-Deadline-Ms", "12332000")
            ]
      it "Works given valid inputs" $ do
        let inputBody = Null
        let response = minResponse basicValidHeaders inputBody
        let (id, outputBody, context) =
              eventResponseToNextData staticContext response
        id `shouldBe` "abc"
        outputBody `shouldBe` inputBody
        context `shouldBe`
          (Right
             (LambdaContext
                { functionName = "name"
                , functionVersion = "version"
                , functionMemorySize = 128
                , logGroupName = "logGroupName"
                , logStreamName = "logStreamName"
                , awsRequestId = "abc"
                , invokedFunctionArn = "arn"
                , xRayTraceId = "123"
                , deadline = posixSecondsToUTCTime 12332
                , clientContext = Nothing
                , identity = Nothing
                }))
      it "has clientContext if it was provided" $ do
        let headers =
              ( "Lambda-Runtime-Client-Context"
              , "{ \"client\": { \"appTitle\": \"title\", \"appVersionName\": \"versionName\", \"appVersionCode\": \"versionCode\", \"appPackageName\": \"packageName\" }, \"custom\": { \"key1\": \"value1\" }, \"environment\": { \"key2\": \"value2\" } }") :
              basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        fmap clientContext context `shouldBe`
          (Right
             (Just
                (ClientContext
                   { client =
                       ClientApplication
                         { appTitle = "title"
                         , appVersionName = "versionName"
                         , appVersionCode = "versionCode"
                         , appPackageName = "packageName"
                         }
                   , custom = singleton "key1" "value1"
                   , environment = singleton "key2" "value2"
                   })))
      it "fails to construct the Context if the client context can't be parsed" $ do
        let headers =
              ("Lambda-Runtime-Client-Context", "{}") : basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        let msg = either id (const (error "Was able to parse a context that should have failed!")) context
        msg `shouldStartWith`
          "Runtime Error: Unable to decode Context from event response.\nCould not JSON decode header Lambda-Runtime-Client-Context: "
      it
        "fails to construct the Context if there are two client context headers" $ do
        let headers =
              [ ("Lambda-Runtime-Client-Context", "{}")
              , ("Lambda-Runtime-Client-Context", "{}")
              ] <>
              basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        fmap clientContext context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Client-Context")
      it "has identity if it was provided" $ do
        let headers =
              ( "Lambda-Runtime-Cognito-Identity"
              , "{ \"identityId\": \"identityId\", \"identityPoolId\": \"identityPoolId\" }") :
              basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        fmap identity context `shouldBe`
          (Right
             (Just
                (CognitoIdentity
                   { identityId = "identityId"
                   , identityPoolId = "identityPoolId"
                   })))
      it
        "fails to construct the Context if the cognito identity can't be parsed" $ do
        let headers =
              ("Lambda-Runtime-Cognito-Identity", "{}") : basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        let msg = either id (const (error "Was able to parse a context that should have failed!")) context
        msg `shouldStartWith`
          "Runtime Error: Unable to decode Context from event response.\nCould not JSON decode header Lambda-Runtime-Cognito-Identity: "
      it
        "fails to construct the Context if there are two cognito identity headers" $ do
        let headers =
              [ ("Lambda-Runtime-Cognito-Identity", "{}")
              , ("Lambda-Runtime-Cognito-Identity", "{}")
              ] <>
              basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        fmap clientContext context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Cognito-Identity")
      it "fails to create the context if trace Id is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
              , ("Lambda-Runtime-Deadline-Ms", "12332000")
              ]
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nMissing response header Lambda-Runtime-Trace-Id")
      it "fails to create the context if trace id has multiple values" $ do
        let headers = ("Lambda-Runtime-Trace-Id", "123") : basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Trace-Id")
      it "fails to create the context if function ARN is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Trace-Id", "123")
              , ("Lambda-Runtime-Deadline-Ms", "12332000")
              ]
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nMissing response header Lambda-Runtime-Invoked-Function-Arn")
      it "fails to create the context if function ARN has multiple values" $ do
        let headers =
              ("Lambda-Runtime-Invoked-Function-Arn", "arn") : basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Invoked-Function-Arn")
      it "fails to create the context if a deadline is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Trace-Id", "123")
              , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
              ]
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nMissing response header Lambda-Runtime-Deadline-Ms")
      it "fails to create the context if the deadline has multiple values" $ do
        let headers =
              ("Lambda-Runtime-Deadline-Ms", "12332000") : basicValidHeaders
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Deadline-Ms")
      it "fails to create the context if the deadline is not a valid timestamp" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Trace-Id", "123")
              , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
              , ("Lambda-Runtime-Deadline-Ms", "not a timestamp")
              ]
        let (_, _, context) =
              eventResponseToNextData staticContext (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nCould not parse deadline")

    describe "properties" $ do
        specify "Header.unfoldCase preserves equality" $
            hedgehog $ do
                -- CI only promises equality over the Latin-1 set
                s <- forAll $ CI.mk <$> Gen.text (Range.linear 1 100) Gen.latin1
                t <- forAll $ Header.unfoldCase s
                s === t

minResponse :: [Header] -> a -> Response a
minResponse headers body =
  Response
    { responseStatus = undefined
    , responseVersion = undefined
    , responseHeaders = headers
    , responseBody = body
    , responseCookieJar = undefined
    , responseClose' = undefined
    }

minJsonResponse :: [Header] -> Response Value
minJsonResponse = flip minResponse Null
