{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (ApiGatewayProxyRequest))
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (ApiGatewayProxyResponse))
import           AWS.Lambda.Runtime                        (pureRuntime)
import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.Text                                 (Text)
import           GHC.Generics                              (Generic)
import           Network.HTTP.Types.Status                 (ok200)

handler :: ApiGatewayProxyRequest -> ApiGatewayProxyResponse Text
handler x =
    ApiGatewayProxyResponse 200 mempty "Hello from Haskell Lambda"

main :: IO ()
main = pureRuntime handler
