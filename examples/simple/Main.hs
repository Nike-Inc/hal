{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest)
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (ApiGatewayProxyResponse), textPlain, ok200)
import           AWS.Lambda.Runtime                        (pureRuntime)

handler :: ApiGatewayProxyRequest -> ApiGatewayProxyResponse
handler _ =
    ApiGatewayProxyResponse ok200 mempty (textPlain "Hello from Haskell Lambda")

main :: IO ()
main = pureRuntime handler
