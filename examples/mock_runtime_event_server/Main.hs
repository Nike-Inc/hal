{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Map (Map)
import           Web.Scotty (json, scotty, get, addHeader, post, body)
import           Data.Aeson         (ToJSON)
import           GHC.Generics       (Generic)
import           Control.Monad.IO.Class   (liftIO)
import System.IO (hPutStrLn, stderr)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)

data IdEvent  = IdEvent { input   :: String } deriving Generic
instance ToJSON IdEvent where

main :: IO ()
main = scotty 8080 $ do
  get "/2018-06-01/runtime/invocation/next" $ do
    addHeader "Lambda-Runtime-Aws-Request-Id" "123"
    addHeader "Lambda-Runtime-Trace-Id" "123"
    addHeader "Lambda-Runtime-Invoked-Function-Arn" "my:arn"
    addHeader "Lambda-Runtime-Deadline-Ms" "0"
    json (IdEvent { input = "hey" })
  post "/2018-06-01/runtime/invocation/123/response" $ do
    liftIO $ show <$> getPOSIXTime >>= hPutStrLn stderr
    json (mempty :: Map String String)
  post "/2018-06-01/runtime/invocation/123/error" $ do
    b <- body
    liftIO $ hPutStrLn stderr "error!"
    liftIO $ hPutStrLn stderr $ unpack b
    json (mempty :: Map String String)
  post "/2018-06-01/runtime/invocation/init/error" $ do
    b <- body
    liftIO $ hPutStrLn stderr "init error!"
    liftIO $ hPutStrLn stderr $ unpack b
    json (mempty :: Map String String)
