module Discourse where

import Prelude

import Control.Error ((??), fmapL)
import Control.Monad.Trans.Except (Except (..), except, runExcept, throwE)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, HMAC, hmacGetDigest)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text (Text, append, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException(..))
import Network.HTTP.Types.URI (renderSimpleQuery, parseSimpleQuery)

import qualified Data.ByteString as B (drop)
import qualified Data.ByteString.Base64 as B64 (decodeLenient)

import Model

-- | Information we send back to Discourse once the user logs in through our
-- UI.
data UserInfo = UserInfo
    { ssoEmail     :: Text
    , ssoId        :: UserId
    , ssoUsername  :: Maybe Text
    , ssoFullName  :: Maybe Text
    , ssoAvatarUrl :: Maybe Text
    , ssoBio       :: Maybe Text
    }

data DiscoursePayload = DiscoursePayload
    { dpNonce   :: ByteString
    , dpUrl     :: Text
    } deriving (Eq, Show)

-- | DataType to handle Discourse Payload error messages.
data DiscoursePayloadError = NoPayload
                           | NoSignature
                           | InvalidSignature
                           | InvalidPayload Text
                           | PayloadMissingNonce
                           | PayloadMissingURL
                           deriving (Eq, Show)

-- | Pretty Formatting of DiscoursePayloadError
getDPErrorMsg :: DiscoursePayloadError -> Text
getDPErrorMsg NoPayload = "No payload was received from Discourse."
getDPErrorMsg NoSignature = "The required validation signature was not received from Discourse."
getDPErrorMsg InvalidSignature = "The validation signature received from Discourse could not be validated."
getDPErrorMsg (InvalidPayload msg) = append "Invalid Payload: " msg
getDPErrorMsg PayloadMissingNonce = "The payload received from Discourse is missing the required nonce."
getDPErrorMsg PayloadMissingURL = "The payload received from Discourse is missing the required return URL."

-- | Type restricted convenience wrapper that computes our HMAC.
hmacSHA256 :: ByteString -> ByteString -> HMAC SHA256
hmacSHA256 = hmac

-- | Given secret known in advance and payload given in the query, compute the
-- HMAC-SHA256, to which Discourse refers as the signature.
generateSig
    :: ByteString -- ^ Secret
    -> ByteString -- ^ Base64 encoded payload
    -> ByteString
generateSig secret payload =
    convertToBase Base16 $ hmacGetDigest $ hmacSHA256 secret payload

-- | This validates the payloads's authenticity (i.e. make sure it's really our
-- trusted local Discourse instance) by using the signature as a proof that it
-- knows the SSO secret. This is done by verifying that the HMAC-SHA256 of the
-- secret and the payload is identical to the signature.
validateSig
    :: ByteString -- ^ SSO secret, same one you specify in Discourse settings
    -> ByteString -- ^ Base64 encoded payload sent by Discourse in the query
    -> ByteString -- ^ Signature sent by Discourse in the query
    -> Bool       -- ^ Whether the computed sig and one passed are identical
validateSig secret payload signature = generateSig secret payload
                                            `constEq`
                                            signature

-- | Get the nonce and the return URL from the payload by decoding from Base64
-- and extracting the parameter values.
--
-- We use lenient decoding here because Discourse doesn't seem to add the
-- necessary padding for strictly by-the-spec Base64 encoding.
parsePayload :: ByteString -> Either DiscoursePayloadError DiscoursePayload
parsePayload b = runExcept $ do
    nonce <- lookup "nonce" params ?? PayloadMissingNonce
    burl <- lookup "return_sso_url" params ?? PayloadMissingURL
    url <- except $ fmapL packError $ decodeUtf8' burl
    return $ DiscoursePayload nonce url
    where
        params = parseSimpleQuery $ B64.decodeLenient b
        packError (DecodeError x _) = InvalidPayload $ pack x
        packError _ = InvalidPayload $ pack ""


-- | Compute Base64 encoded payload to send back to Discourse after login
userInfoPayload
    :: ByteString -- ^ Raw nonce string we extracted from input payload
    -> UserInfo   -- ^ Info about the user we pass back to Discourse
    -> ByteString
userInfoPayload nonce uinfo = convertToBase Base64URLUnpadded $
                                            renderSimpleQuery False query
    where
        query = mapMaybe (uncurry (fmap . (,)))
            [ ("nonce"      , Just nonce)
            , ("email"      , Just $ encodeUtf8 $ ssoEmail uinfo)
            , ("external_id", Just $ encodeUtf8 $ pack $ show $ ssoId uinfo)
            , ("username"   , encodeUtf8 <$> ssoUsername uinfo)
            , ("name"       , encodeUtf8 <$> ssoFullName uinfo)
            , ("avatar_url" , encodeUtf8 <$> ssoAvatarUrl uinfo)
            , ("bio"        , encodeUtf8 <$> ssoBio uinfo)
            ]
