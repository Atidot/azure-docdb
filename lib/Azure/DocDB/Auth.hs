{-# LANGUAGE OverloadedStrings #-}

module Azure.DocDB.Auth (
  DocDBSignature(..),
  SigningParams(..),
  MSDate(..),
  signRequestInfo,
  signingPayload,
  ) where


import qualified Crypto.Hash.Algorithms as CH
import qualified Crypto.MAC.HMAC as HM
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Network.HTTP.Types (StdMethod(..), renderQuery, queryTextToQuery)
import           Web.HttpApiData (ToHttpApiData(..))


-- | Parameters needed to generate a signing header
data SigningParams = SigningParams {
  spMethod :: StdMethod,
  spResourceType :: T.Text,
  spPath :: T.Text,
  spWhen :: MSDate
  } deriving (Eq, Ord)


newtype MSDate = MSDate UTCTime deriving (Eq, Ord)

-- | Computed signature data, sufficient to generate an authorization header
data DocDBSignature = DocDBSignature
  { dbSigType :: T.Text
  , dbSigVer :: T.Text
  , dbSigSig :: T.Text
  } deriving (Eq, Ord)


instance ToHttpApiData DocDBSignature where
  -- e.g.:  type=master&ver=1.0&sig=5mDuQBYA0kb70WDJoTUzSBMTG3owkC0/cEN4fqa18/s=
  toHeader sig = renderQuery False makeQS
    where
      makeQS = queryTextToQuery
        [ ("type", Just (dbSigType sig))
        , ("ver", Just (dbSigVer sig))
        , ("sig", Just (dbSigSig sig))
        ]
  toUrlPiece = T.decodeUtf8 . toHeader


instance Show DocDBSignature where
  show = B8.unpack . toHeader


-- | Compute an HMAC (base64 encoded)
computeHMAC256 :: B.ByteString -> T.Text -> T.Text
computeHMAC256 key text =
  T.decodeUtf8 . B64.encode . B.pack . BA.unpack . HM.hmacGetDigest $ hmacedValue
  where
    hmacedValue :: HM.HMAC CH.SHA256
    hmacedValue = HM.hmac key (T.encodeUtf8 text)


-- | Build a signature from the parameters and the signing key
signRequestInfo :: B.ByteString -> SigningParams -> DocDBSignature
signRequestInfo key params = DocDBSignature {
  dbSigType = "master",
  dbSigVer = "1.0",
  dbSigSig = signature
  }
  where
    signature = computeHMAC256 key $ signingPayload params


-- | Format a date per RFC 7321
-- e.g.  Tue, 01 Nov 1994 08:12:31 GMT
instance Show MSDate where
  show (MSDate d) = formatTime defaultTimeLocale "%a, %0d %b %Y %H:%M:%S GMT" d

instance ToHttpApiData MSDate where
  toHeader = B8.pack . show
  toQueryParam = T.pack . show


-- | Get the text which would need to be signed.
signingPayload :: SigningParams -> T.Text
signingPayload (SigningParams method resourceType path when) =
  mconcat . appendNL $ parts
  where
    parts = [lcMethod, lcRecourceType, path, lcWhen, ""]
    appendNL pieces = pieces >>= flip (:) ["\n"]
    lcMethod = T.toLower . T.pack . show $ method
    lcWhen = T.toLower . T.pack . show $ when
    lcRecourceType = T.toLower resourceType
