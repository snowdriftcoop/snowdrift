-- Discourse SSO implementation based on documentation at
-- https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045

module DiscourseSpec where

import TestImport

import Discourse
    (DiscoursePayload(DiscoursePayload, dpNonce, dpUrl), parsePayload,
        validateSig)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray.Encoding (Base(Base16, Base64URLUnpadded), convertToBase)
import Data.Text.Arbitrary ()
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, choose, oneof, vectorOf)
import Test.QuickCheck.Property ((==>))

import qualified Data.ByteString.Char8 as Char8

spec :: Spec
spec = do
    prop "validateSig" $ \secret payload ->
        secret /= [] && payload /= [] ==> do
            let secret' = pack secret
                payload' = pack payload

            validateSig secret' payload'
                (base16 (hmac secret' payload' :: HMAC SHA256))

    prop "parsePayload" $ \(Nonce nonce) (Url url) -> do
        let payload = "nonce=" <> nonce <> "&return_sso_url=" <> encodeUtf8 url
        parsePayload (base64 payload)
            `shouldBe`
            Right (DiscoursePayload
                { dpNonce = nonce
                , dpUrl   = url
                })

-- 16 bytes of [0-9A-F]
newtype Nonce
    = Nonce ByteString
    deriving (Eq, Show)

instance Arbitrary Nonce where
  arbitrary =
      Nonce . Char8.pack <$>
          vectorOf 16 (oneof [choose ('0','9'), choose ('A', 'F')])

-- URL-ish thing (i.e. avoid characters that can't appear in URLs, such as '+')
newtype Url
    = Url Text
    deriving (Eq, Show)

instance Arbitrary Url where
  arbitrary = Url . pack <$> vectorOf 10 (choose ('a', 'z'))

base16 :: (ByteArrayAccess a, ByteArray b) => a -> b
base16 = convertToBase Base16

base64 :: (ByteArrayAccess a, ByteArray b) => a -> b
base64 = convertToBase Base64URLUnpadded
