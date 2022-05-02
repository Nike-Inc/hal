{-# LANGUAGE CPP #-}
module Gen.Header where

import           Control.Monad.Trans.State
import           Data.CaseInsensitive      (CI)
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 (toUpper, toLower)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as H
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup            ((<>))
#endif
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Traversable          (for)
import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Network.HTTP.Types.Header

multiValueHeaders :: Gen (HashMap (CI Text) [Text])
multiValueHeaders = multiValueHeadersExcept S.empty

multiValueHeadersExcept :: Set (CI Text) -> Gen (HashMap (CI Text) [Text])
multiValueHeadersExcept exclusions = H.fromListWith (<>) <$>
    evalStateT
        (Gen.list (Range.linear 1 30) $ singleHeaderExcept exclusions)
        S.empty

singleHeaderExcept :: Set (CI Text) -> StateT (Set (CI Text)) Gen (CI Text, [Text])
singleHeaderExcept exclusions = Gen.choice [fresh, clash] where
    fresh = do
        headers <- get
        newHeader <- Gen.filterT
            (\h -> not $ S.member h headers || S.member h exclusions)
            header
        put $ S.insert newHeader headers
        headerVal <- val
        pure (newHeader, [headerVal])
    clash = do
        headers <- get
        if null headers
        then fresh
        else do
            header <- Gen.filterT
                (\h -> not $ S.member h exclusions)
                (Gen.element (S.toList headers))
                >>= unfoldCase
            headerVal <- val
            pure (header, [headerVal])

header :: MonadGen g => g (CI Text)
header = Gen.choice
    [ CI.mk <$> Gen.text (Range.linear 1 15) Gen.alpha
    , standardHeader
    ]

standardHeader :: MonadGen g => g (CI Text)
standardHeader = CI.map TE.decodeUtf8 <$> Gen.element
    [ hAccept
    , hAcceptCharset
    , hAcceptEncoding
    , hAcceptLanguage
    , hAcceptRanges
    , hAge
    , hAllow
    , hAuthorization
    , hCacheControl
    , hConnection
    , hContentEncoding
    , hContentLanguage
    , hContentLength
    , hContentLocation
    , hContentMD5
    , hContentRange
    , hContentType
    , hDate
    , hETag
    , hExpect
    , hExpires
    , hFrom
    , hHost
    , hIfMatch
    , hIfModifiedSince
    , hIfNoneMatch
    , hIfRange
    , hIfUnmodifiedSince
    , hLastModified
    , hLocation
    , hMaxForwards
    , hOrigin
    , hPragma
    , hPrefer
    , hPreferenceApplied
    , hProxyAuthenticate
    , hProxyAuthorization
    , hRange
    , hReferer
    , hRetryAfter
    , hServer
    , hTE
    , hTrailer
    , hTransferEncoding
    , hUpgrade
    , hUserAgent
    , hVary
    , hVia
    , hWWWAuthenticate
    , hWarning
    , hContentDisposition
    , hMIMEVersion
    , hCookie
    , hSetCookie
    ]

-- | Create some permutation of the input text, that compares as equal
-- under 'CI'.
unfoldCase :: MonadGen g => CI Text -> g (CI Text)
unfoldCase t = CI.mk . T.pack <$> for (T.unpack $ CI.original t) flipCase
    where flipCase c = Gen.element [toUpper c, toLower c]

val :: MonadGen g => g Text
val = Gen.text (Range.linear 1 15) Gen.unicode
