module AWS.Lambda.Runtime (
  ioLambdaRuntime,
  pureLambdaRuntime,
  simpleLambdaRuntime
) where

import           AWS.Lambda.RuntimeClient (getBaseRuntimeRequest, getNextEvent, postSuccess,
                                           postHandlerError, postRuntimeError)
import           Data.Bifunctor           (first)
import           Control.Exception        (displayException, evaluate, SomeException, try, throw, catch)
import           Control.Monad            (forever, join)
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Char8 as BSC
import           Network.HTTP.Simple      (getResponseBody, getResponseHeader, JSONException(..), Request)
import           System.Environment       (setEnv)

runtimeLoop :: (FromJSON event, ToJSON result) => Request ->
  (event -> IO (Either String result)) -> IO ()
runtimeLoop baseRuntimeRequest fn = do
  -- Get an event
  nextRes <- getNextEvent baseRuntimeRequest

  -- Propagate the tracing header
  let traceId = head $ getResponseHeader "Lambda-Runtime-Trace-Id" nextRes
  setEnv "_X_AMZN_TRACE_ID" (BSC.unpack traceId)

  -- TODO: Create a context object
  let reqId = head $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" nextRes

  result <- case getResponseBody nextRes of
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
      return $ join $ first (displayException :: SomeException -> String) caughtResult

  case result of
    Right r ->
      -- TODO: 4xx errors should propogate the returned error to the postHandlerError.
      -- It appears that after successful event retreival, postRunTimeError cannot be called.
      -- It gives this error message: "Transition from STATE_INVOKE_NEXT to STATE_INIT_ERROR is not allowed."
      -- Where should that logic be handled?
      postSuccess baseRuntimeRequest reqId r
    Left e -> postHandlerError baseRuntimeRequest reqId e

-- | For functions with IO that can fail in a pure way (or via throwM).
ioLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> IO (Either String result)) -> IO ()
ioLambdaRuntime fn = do
  -- Retreive settings; if this fails there is no hope of any useful recovery
  baseRuntimeRequest <- getBaseRuntimeRequest
  catch
    (forever $ runtimeLoop baseRuntimeRequest fn)
    (\ex -> do
      -- Catching all exceptions is _usually_ bad practice,
      -- but this is clearly appropriate as it our top level handler
      let msg = displayException (ex :: SomeException)
      _ <- postRuntimeError baseRuntimeRequest msg
      error "Runtime error, exiting now."
    )

-- | For pure functions that can still fail.
pureLambdaRuntime :: (FromJSON event, ToJSON result) =>
  (event -> Either String result) -> IO ()
pureLambdaRuntime fn = ioLambdaRuntime (evaluate . fn)

-- | For pure functions that can never fail.
simpleLambdaRuntime :: (FromJSON event, ToJSON result) => (event -> result) -> IO ()
simpleLambdaRuntime fn = pureLambdaRuntime (Right . fn)
