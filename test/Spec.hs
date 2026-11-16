{-# LANGUAGE CPP #-}
import           AWS.Lambda.Context                 (ClientApplication (..),
                                                     ClientContext (..),
                                                     CognitoIdentity (..),
                                                     LambdaContext (..))
import qualified Gen.Header                         as Header
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest.Spec as ProxyRequest
import qualified AWS.Lambda.Events.ApiGateway.ProxyResponse.Spec as ProxyResponse

import qualified AWS.Lambda.Events.EventBridge.Spec as EventBridge
import qualified AWS.Lambda.Events.Kafka.Spec       as Kafka
import qualified AWS.Lambda.Events.SQS.Spec         as SQS
import           AWS.Lambda.Internal                (StaticContext (..))
import           AWS.Lambda.RuntimeClient.Internal  (NextData (..),
                                                     eventResponseToNextData,
                                                     generateRootTraceId,
                                                     mkRootTraceId)
import           Control.Exception                  (evaluate)
import           Data.Aeson                         (Value (Null, String))
import qualified Data.CaseInsensitive               as CI
import           Data.Map                           (singleton)
import           Data.Maybe                         (isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup                     ((<>))
#endif
import           Data.Time.Clock.POSIX              (getPOSIXTime,
                                                     posixSecondsToUTCTime)
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import           Network.HTTP.Client.Internal       (Response (..))
import           Network.HTTP.Types                 (Header)
import           System.Mem                         (performMajorGC)
import           System.Mem.Weak                    (deRefWeak, mkWeakPtr)
import           Test.Hspec                         (anyErrorCall, describe, it,
                                                     shouldBe, shouldSatisfy,
                                                     shouldStartWith,
                                                     shouldThrow, specify)
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
      describe "SQS" SQS.spec

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
      let fallbackTraceId =
            "Root=1-5821d5ce-0001234500000000000000ab;Sampled=0" :: Text
      it "Works given valid inputs" $ do
        let inputBody = Null
        let response = minResponse basicValidHeaders inputBody
        let NextData id traceId outputBody context =
              eventResponseToNextData staticContext fallbackTraceId response
        id `shouldBe` "abc"
        traceId `shouldBe` Just "123"
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
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
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
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        let msg = either id (const (error "Was able to parse a context that should have failed!")) context
        msg `shouldStartWith`
          "Runtime Error: Unable to decode Context from event response.\nCould not JSON decode header Lambda-Runtime-Client-Context: "
      it "still has the request and trace ids if the Context can't be constructed" $ do
        let headers =
              ("Lambda-Runtime-Client-Context", "{}") : basicValidHeaders
        let NextData id traceId _ context =
              eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        id `shouldBe` "abc"
        traceId `shouldBe` Just "123"
        context `shouldSatisfy` either (const True) (const False)
      it "carries the fallback trace id if the header is missing" $ do
        let headers =
              filter ((/=) "Lambda-Runtime-Trace-Id" . fst) basicValidHeaders
        nextTraceId (eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers))
          `shouldBe` Just fallbackTraceId
      it "fails as soon as it is evaluated if there is no request id" $ do
        let headers =
              filter ((/=) "Lambda-Runtime-Aws-Request-Id" . fst) basicValidHeaders
        evaluate (eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers))
          `shouldThrow` anyErrorCall
      it "fails as soon as it is evaluated if there is more than one request id" $ do
        let headers = ("Lambda-Runtime-Aws-Request-Id", "def") : basicValidHeaders
        evaluate (eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers))
          `shouldThrow` anyErrorCall
      -- The runtime holds the request id for the whole invocation, and a
      -- handler may hold the context.  Neither may keep the event (which can
      -- be many megabytes once parsed) from being collected.
      --
      -- A weak pointer to the event tells us whether anything still reaches
      -- it after a GC.  The event is made at run time so that it is a fresh
      -- heap object, which the GC is able to collect.
      describe "event retention" $ do
        it "can tell that an event is still reachable (sanity check)" $ do
          event <- evaluate . String . T.pack . show =<< getPOSIXTime
          weak <- mkWeakPtr event Nothing
          NextData _ _ heldEvent _ <-
            evaluate $
            eventResponseToNextData staticContext fallbackTraceId (minResponse basicValidHeaders event)
          performMajorGC
          reachable <- isJust <$> deRefWeak weak
          reachable `shouldBe` True
          -- Looked at only now, so it had to be held across the GC.
          heldEvent `shouldBe` event
        it "does not keep the event reachable through the request id or the context" $ do
          event <- evaluate . String . T.pack . show =<< getPOSIXTime
          weak <- mkWeakPtr event Nothing
          NextData heldId _ _ heldContext <-
            evaluate $
            eventResponseToNextData staticContext fallbackTraceId (minResponse basicValidHeaders event)
          performMajorGC
          reachable <- isJust <$> deRefWeak weak
          reachable `shouldBe` False
          -- Looked at only now, so they had to be held across the GC.
          heldId `shouldBe` "abc"
          fmap awsRequestId heldContext `shouldBe` Right "abc"
      it
        "fails to construct the Context if there are two client context headers" $ do
        let headers =
              [ ("Lambda-Runtime-Client-Context", "{}")
              , ("Lambda-Runtime-Client-Context", "{}")
              ] <>
              basicValidHeaders
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        fmap clientContext context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Client-Context")
      it "has identity if it was provided" $ do
        let headers =
              ( "Lambda-Runtime-Cognito-Identity"
              , "{ \"identityId\": \"identityId\", \"identityPoolId\": \"identityPoolId\" }") :
              basicValidHeaders
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
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
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
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
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        fmap clientContext context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Cognito-Identity")
      it "uses the fallback trace Id if trace Id is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
              , ("Lambda-Runtime-Deadline-Ms", "12332000")
              ]
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        fmap xRayTraceId context `shouldBe` Right fallbackTraceId
      it "fails to create the context if trace id has multiple values" $ do
        let headers = ("Lambda-Runtime-Trace-Id", "123") : basicValidHeaders
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Trace-Id")
      it "fails to create the context if function ARN is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Trace-Id", "123")
              , ("Lambda-Runtime-Deadline-Ms", "12332000")
              ]
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nMissing response header Lambda-Runtime-Invoked-Function-Arn")
      it "fails to create the context if function ARN has multiple values" $ do
        let headers =
              ("Lambda-Runtime-Invoked-Function-Arn", "arn") : basicValidHeaders
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nToo many values for header Lambda-Runtime-Invoked-Function-Arn")
      it "fails to create the context if a deadline is not provided" $ do
        let headers =
              [ ("Lambda-Runtime-Aws-Request-Id", "abc")
              , ("Lambda-Runtime-Trace-Id", "123")
              , ("Lambda-Runtime-Invoked-Function-Arn", "arn")
              ]
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nMissing response header Lambda-Runtime-Deadline-Ms")
      it "fails to create the context if the deadline has multiple values" $ do
        let headers =
              ("Lambda-Runtime-Deadline-Ms", "12332000") : basicValidHeaders
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
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
        let context =
              nextContext $ eventResponseToNextData staticContext fallbackTraceId (minJsonResponse headers)
        context `shouldBe`
          (Left
             "Runtime Error: Unable to decode Context from event response.\nCould not parse deadline")

    describe "Root trace Id" $ do
      it "formats the time and identifier as zero-padded hex" $ do
        mkRootTraceId 1478612430 0x12345 0xab `shouldBe`
          "Root=1-5821d5ce-0001234500000000000000ab;Sampled=0"
      it "generates a trace Id of the expected shape" $ do
        traceId <- generateRootTraceId
        T.length traceId `shouldBe` 50
        T.isPrefixOf "Root=1-" traceId `shouldBe` True
        T.isSuffixOf ";Sampled=0" traceId `shouldBe` True

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
