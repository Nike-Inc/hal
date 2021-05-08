module AWS.Lambda.Events.ApiGateway.ProxyRequest.Gen.Parameters where

import           Control.Monad.Trans.State
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as H
import           Data.Semigroup            ((<>))
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

multiParameters :: Gen (HashMap Text [Text])
multiParameters = H.fromListWith (<>) <$>
    evalStateT (Gen.list (Range.linear 1 10) parameter) S.empty

parameter :: StateT (Set Text) Gen (Text, [Text])
parameter = Gen.choice [fresh, clash] where
    fresh = do
        vars <- get
        newVar <- Gen.filterT (not . (`S.member` vars)) var
        put $ S.insert newVar vars
        v <- val
        pure (newVar, [v])
    clash = do
        vars <- get
        if null vars
        then fresh
        else do
            var <- Gen.element $ S.toList vars
            v <- val
            pure (var, [v])

var :: MonadGen g => g Text
var = Gen.text (Range.linear 1 10) Gen.alpha

val :: MonadGen g => g Text
val = Gen.text (Range.linear 1 15) Gen.unicode
