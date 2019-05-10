{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (ApiGatewayProxyRequest))
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (ApiGatewayProxyResponse), textPlain)
import           AWS.Lambda.Runtime                        (pureRuntime)
import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.Text                                 (Text)
import           GHC.Generics                              (Generic)
import           Network.HTTP.Types.Status                 (ok200)

handler :: ApiGatewayProxyRequest -> ApiGatewayProxyResponse
handler x =
    ApiGatewayProxyResponse ok200 mempty (textPlain "Hello from Haskell Lambda")

main :: IO ()
main = pureRuntime handler
