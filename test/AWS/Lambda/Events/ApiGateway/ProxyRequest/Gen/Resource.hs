{-# LANGUAGE LambdaCase #-}

module AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen.Resource where

import           Control.Monad             (foldM)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Foldable             (toList)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as H
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

data ResourceSegment = Literal Text | Variable Text deriving (Eq, Show)

segments :: Gen (NonEmpty ResourceSegment)
segments = flip evalStateT S.empty
    . fmap NE.fromList
    . Gen.list (Range.linear 1 10)
    $ Gen.choice
        [ Literal <$> Gen.text (Range.linear 1 10) Gen.alpha
        , Variable <$> var
        ]

-- | Generate a variable mapping for a resource path.
vars :: Foldable t => t ResourceSegment -> Gen (HashMap Text Text)
vars = foldM addVar H.empty
    where
        addVar m = \case
            Literal{} -> pure m
            Variable v -> do
                value <- Gen.text (Range.linear 1 20) Gen.unicode
                pure $ H.insert v value m

-- | Instantiate vars in a resource path.
path :: HashMap Text Text -> NonEmpty ResourceSegment -> Text
path vars = ("/" <>) . T.intercalate "/" . toList . fmap toText
    where
        toText = \case
            Literal t -> t
            Variable v -> vars H.! v

-- | Render a resource path with @{var}@ to indicate variables
resource :: NonEmpty ResourceSegment -> Text
resource = ("/" <>) . T.intercalate "/" . toList . fmap toText
    where
        toText = \case
            Literal t -> t
            Variable v -> "{" <> v <> "}"

-- | Generate one variable name for a resource path, and don't reuse
-- variable names.
var :: StateT (S.Set Text) Gen Text
var = do
    vars <- get
    newVar <- Gen.filterT (not . (`S.member` vars)) $
        Gen.text (Range.linear 1 10) Gen.alpha
    put $ S.insert newVar vars
    pure newVar
