module Discourse where

import Prelude

import Control.Monad.Trans.Except
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
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
validateSig secret payload signature = generateSig secret payload == signature

-- | Get the nonce and the return URL from the payload by decoding from Base64
-- and extracting the parameter values.
--
-- We use lenient decoding here because Discourse doesn't seem to add the
-- necessary padding for strictly by-the-spec Base64 encoding.
parsePayload :: ByteString -> Either String (ByteString, Text)
parsePayload b = runExcept $ do
    let params = parseSimpleQuery $ B64.decodeLenient b
    nonce <- maybe (throwE "Nonce is missing") return $ lookup "nonce" params
    burl <- maybe (throwE "URL is missing") return $
        lookup "return_sso_url" params
    let mapLeft f (Left x)  = Left $ f x
        mapLeft _ (Right x) = Right x
    url <- except $ mapLeft show $ decodeUtf8' burl
    return (nonce, url)

-- | Compute Base64 encoded payload to send back to Discourse after login
userInfoPayload
    :: ByteString -- ^ Raw nonce string we extracted from input payload
    -> UserInfo   -- ^ Info about the user we pass back to Discourse
    -> ByteString
userInfoPayload nonce uinfo =
    let query = catMaybes $ map (\ (name, mval) -> fmap (name,) mval)
            [ ("nonce"      , Just nonce)
            , ("email"      , Just $ encodeUtf8 $ ssoEmail uinfo)
            , ("external_id", Just $ encodeUtf8 $ pack $ show $ ssoId uinfo)
            , ("username"   , fmap encodeUtf8 $ ssoUsername uinfo)
            , ("name"       , fmap encodeUtf8 $ ssoFullName uinfo)
            , ("avatar_url" , fmap encodeUtf8 $ ssoAvatarUrl uinfo)
            , ("bio"        , fmap encodeUtf8 $ ssoBio uinfo)
            ]
    in  convertToBase Base64 $ renderSimpleQuery False query
