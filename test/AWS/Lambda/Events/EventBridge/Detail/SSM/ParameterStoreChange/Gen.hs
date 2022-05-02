{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange.Gen where

import           AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange (Operation(..), ParameterStoreChange(..))
import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Prelude        hiding (id)

parameterStoreChange :: Gen ParameterStoreChange
parameterStoreChange = do
  operation <- Gen.element [Create, Update, Delete, LabelParameterVersion]
  name <- Gen.text (Range.linear 1 200) Gen.unicode
  type_ <- Gen.enumBounded
  description <- Gen.maybe $ Gen.text (Range.linear 1 200) Gen.unicode

  pure ParameterStoreChange
    { operation
    , name
    , type_
    , description
    }
