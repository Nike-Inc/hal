{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : AWS.Lambda.Events.ApiGatewayProxyResponse
Description : Data types that represent typical lambda responses
Copyright   : (c) Nike, Inc., 2018
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}
module AWS.Lambda.Events.ApiGatewayProxyResponse
    ( ApiGatewayProxyResponse(..)
    ) where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic (..))

data ApiGatewayProxyResponse a = ApiGatewayProxyResponse
    { statusCode :: Int
    , headers    :: [(Text, Text)]
    , body       :: a
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiGatewayProxyResponse a)
