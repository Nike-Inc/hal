{-|
Module      : AWS.Lambda.Log.Internal
Description : Internal machinery for writing log lines.
Copyright   : (c) Nike, Inc., 2026
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com
Stability   : unstable
-}

module AWS.Lambda.Log.Internal (
  logLine,
  writeLogLine,
) where

import           AWS.Lambda.Context   (LogFormat (..), LogLevel (..))
import           Data.Aeson           (pairs, (.=))
import           Data.Aeson.Encoding  (encodingToLazyByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSW
import           Data.Text            (Text, pack)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import           System.IO            (stderr)

-- | Write a log line to stderr.  A strict 'BS.hPut' holds the handle's lock
-- for the whole line, so lines from concurrent threads don't interleave.
writeLogLine :: LogFormat -> LogLevel -> Text -> Text -> IO ()
writeLogLine format level reqId msg = do
  now <- getCurrentTime
  BS.hPut stderr $ logLine format now level reqId msg <> "\n"

-- | A log line, without its newline.  In the JSON format, the timestamp must
-- be RFC 3339 (as aeson renders 'UTCTime'), or Lambda treats the line as
-- INFO.  The text format is the one Hal has always used for invocation
-- errors, which 'show's the message to keep it on one line.
logLine :: LogFormat -> UTCTime -> LogLevel -> Text -> Text -> BS.ByteString
logLine JSON timestamp level reqId msg =
  BSW.toStrict $ encodingToLazyByteString $ pairs $
       "timestamp" .= timestamp
    <> "level" .= levelName level
    <> "message" .= msg
    <> "requestId" .= reqId
logLine PlainText _ level _ msg =
  encodeUtf8 $ levelName level <> " Message: " <> pack (show msg)

levelName :: LogLevel -> Text
levelName Trace = "TRACE"
levelName Debug = "DEBUG"
levelName Info  = "INFO"
levelName Warn  = "WARN"
levelName Error = "ERROR"
levelName Fatal = "FATAL"
