{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import AWS.Lambda.Events
       (ApiGatewayProxyRequest(..),
        ApiGatewayProxyResponse(ApiGatewayProxyResponse))
import AWS.Lambda.Runtime (pureRuntime)
import Data.Aeson (FromJSON, ToJSON)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (ok200)

handler :: ApiGatewayProxyRequest String -> ApiGatewayProxyResponse String
handler x =
    traceShow
        x
        (ApiGatewayProxyResponse ok200 mempty "Hello from Haskell Lambda")

main :: IO ()
main = pureRuntime handler
