{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module AWS.Lambda.Events.EventBridge.Gen where

import           AWS.Lambda.Events.EventBridge (EventBridgeEvent'(..),
                                                EventBridgeEvent)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime)
import           Data.Traversable              (for)
import           Hedgehog                      (Gen)
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Prelude                       hiding (id)

eventBridgeEvent :: Gen EventBridgeEvent
eventBridgeEvent = do
  let version = "0"
  id <- uuid
  detailType <- Gen.text (Range.linear 1 200) Gen.unicode
  source <- Gen.text (Range.linear 1 200) Gen.unicode
  account <- awsAccount
  time <- utcTime
  region <- awsRegion
  resources <- Gen.list (Range.linear 1 5) resource
  let detail = "{}"

  pure EventBridgeEvent
    { version
    , id
    , detailType
    , source
    , account
    , time
    , region
    , resources
    , detail
    }

uuid :: Gen Text
uuid = do
  uuidParts <- for [ 7, 4, 4, 4, 12, 2 ] $ \n ->
    Gen.text (Range.singleton n) Gen.hexit
  pure $ T.intercalate "-" uuidParts

utcTime :: Gen UTCTime
utcTime = do
  int <- Gen.int64
    (Range.exponentialFrom 1600000000000 1650000000000 1700000000000)
  let (seconds, millis) = int `divMod` 1000
      posixSeconds = fromIntegral seconds + fromIntegral millis / 1000
  pure $ posixSecondsToUTCTime posixSeconds

awsAccount :: Gen Text
awsAccount = Gen.text (Range.singleton 12) Gen.digit

awsRegion :: Gen Text
awsRegion = Gen.element ["ap-south-1", "us-east-2", "eu-west-3"]

resource :: Gen Text
resource = do
  acc <- awsAccount
  svc <- Gen.text (Range.linear 1 10) Gen.lower
  reg <- awsRegion
  res <- Gen.text (Range.linear 1 20) Gen.alphaNum
  pure $ T.intercalate ":" ["arn:aws", svc, reg, acc, res]
