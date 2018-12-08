module AWS.Lambda.Runtime (
  ioLambdaRuntime,
  pureLambdaRuntime,
  simpleLambdaRuntime
) where

import           Data.Bifunctor        (first)
import           Control.Exception     (displayException, evaluate, SomeException, try, throw)
import           Control.Monad         (forever, join)
import           Data.Aeson            (FromJSON (..), ToJSON (..))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           GHC.Generics          (Generic (..))
import           Network.HTTP.Simple   (getResponseBody, getResponseHeader,
                                        httpJSONEither, httpNoBody, parseRequest,
                                        setRequestBodyJSON, setRequestHeader,
                                        setRequestMethod, setRequestPath, JSONException(..))
import           System.Environment    (getEnv, setEnv)

-- | Lambda runtime error that we pass back to AWS
data LambdaError = LambdaError
  { errorMessage :: String,
    errorType    :: String,
    stackTrace   :: [String]
  } deriving (Show, Generic)

instance ToJSON LambdaError

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = forever $ do
  -- Retreive settings
  awsLambdaRuntimeApi <- getEnv "AWS_LAMBDA_RUNTIME_API"
  baseRequest <- parseRequest $ "http://" ++ awsLambdaRuntimeApi

  -- Get an event
  nextRes <- httpJSONEither $ setRequestPath "2018-06-01/runtime/invocation/next" baseRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  -- TODO: Create a context object
  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  let eventOrJSONEx = getResponseBody nextRes
  result <- case eventOrJSONEx of
    -- If the event was invalid JSON, it's an unrecoverable runtime error
    Left ex@(JSONParseException _ _ _) -> throw ex

    -- If we failed to convert the JSON to the handler's event type, we consider
    -- it a handler error without ever calling it.
    Left ex@(JSONConversionException _ _ _) -> evaluate $ Left $ displayException ex

    -- Otherwise, we'll pass the event into the handler
    Right event -> do
      {- Note1: catching like this is _usually_ considered bad practice, but this is a true
           case where we want to both catch all errors and propogate information about them.
           See: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#g:4
         Note2: This might make BYOMonad harder, we're really exepecting an Either now.
           catch is the alternative, but has us working harder with the exception itself.
      -}
      -- Put the exception in an Either, so we get nested Eithers
      caughtResult <- try (fn event)
      -- Map the outer Either (via first) so they are both of `Either String a`, then collapse them (via join)
      let handlerResult = join $ first (displayException :: SomeException -> String) caughtResult
      return handlerResult

  case result of
    Right r -> do
      -- Handle the response (successes)
      let successUrl
            = setRequestBodyJSON r
            $ setRequestMethod "POST"
            $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/response"])
            $ baseRequest
      _ <- httpNoBody successUrl

      -- TODO: Handle errors
      return ()

    Left e -> do
      let failureUrl
            = setRequestBodyJSON (LambdaError { errorMessage = e, stackTrace = [], errorType = "User"})
            $ setRequestHeader "Content-Type" ["application/vnd.aws.lambda.error+json"]
            $ setRequestMethod "POST"
            $ setRequestPath (BS.concat ["2018-06-01/runtime/invocation/", reqId, "/error"])
            $ baseRequest
      _ <- httpNoBody failureUrl
      return ()

-- | For pure functions that can still fail.
pureLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
pureLambdaRuntime fn = ioLambdaRuntime (evaluate . fn)

-- | For pure functions that can never fail.
simpleLambdaRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
simpleLambdaRuntime fn = pureLambdaRuntime (Right . fn)
