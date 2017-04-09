module HandlerSpec (spec) where

import TestImport

import Factories (createUser)
import Settings (AppSettings(..))

import qualified Discourse

import Data.ByteArray.Encoding (Base(Base16, Base64URLUnpadded), convertToBase)
import Network.HTTP.Types.Status (status303)
import Network.HTTP.Types.URI (decodePath)
import Network.Wai.Test (SResponse(..))

spec :: Spec
spec = withApp $ do
    describe "getRobotsR" getRobotsSpec
    describe "getFaviconR" getFaviconSpec
    describe "getWelcomeR" getWelcomeSpec
    describe "getDashboardR" getDashboardSpec
    describe "getDiscourseR" getDiscourseSpec

getRobotsSpec :: SpecWith (TestApp App)
getRobotsSpec = do
    it "gives a 200" $ do
        get RobotsR
        statusIs 200
    it "has correct User-agent" $ do
        get RobotsR
        bodyContains "User-agent: *"

getFaviconSpec :: SpecWith (TestApp App)
getFaviconSpec = do
    it "gives a 200" $ do
        get FaviconR
        statusIs 200

getWelcomeSpec :: SpecWith (TestApp App)
getWelcomeSpec = do
    describe "browsing anonymously" $ do
        it "loads" $ do
            get WelcomeR
            statusIs 200
        it "has a link to /how-it-works" $ do
            get WelcomeR
            htmlHasLink HowItWorksR
    describe "browsing while logged in" $ do
        it "loads" $ do
            dummyLogin
            printBody
            get WelcomeR
            statusIs 200

getDashboardSpec :: SpecWith (TestApp App)
getDashboardSpec = do
    it "requires login" $
        assertNeedsAuth $ do
          setMethod "GET"
          setUrl DashboardR

getDiscourseSpec :: SpecWith (TestApp App)
getDiscourseSpec = do
    it "returns 400 when missing sso param" $ do
        -- GET /discourse/sso
        get DiscourseR

        statusIs 400

        bodyContains (unpack (Discourse.getDPErrorMsg Discourse.NoPayload))

    it "returns 400 when missing sig param" $ do
        -- GET /discourse/sso?sso=foo
        request $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" "foo"

        statusIs 400

        bodyContains (unpack (Discourse.getDPErrorMsg Discourse.NoSignature))

    it "returns 400 when signature doesn't validate" $ do
        -- GET /discourse/sso?sso=foo&sig=bar
        request $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" "foo"
            addGetParam "sig" "bar"

        statusIs 400

        bodyContains
          (unpack (Discourse.getDPErrorMsg Discourse.InvalidSignature))

    it "returns 400 when the payload doesn't contain 'nonce'" $ do
        let payload = encodePayload "foo"

        sig <- payloadSig payload

        -- GET /discourse/sso=foo&sig=hmac(foo)
        request $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" (decodeUtf8 payload)
            addGetParam "sig" sig

        statusIs 400

        bodyContains
          (unpack (Discourse.getDPErrorMsg Discourse.PayloadMissingNonce))

    it "returns 400 when the payload doesn't contain 'return_sso_url'" $ do
        let payload = encodePayload "nonce=foo"

        sig <- payloadSig payload

        -- GET /discourse/sso=base64(nonce=foo)&sig=hmac(base64(nonce=foo))
        request $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" (decodeUtf8 payload)
            addGetParam "sig" sig

        statusIs 400

        bodyContains
          (unpack (Discourse.getDPErrorMsg Discourse.PayloadMissingURL))

    it "redirects to login page if user is not logged in" $ do
        let payload = encodePayload "nonce=foo&return_sso_url=bar"

        sig <- payloadSig payload

        assertNeedsAuth $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" (decodeUtf8 payload)
            addGetParam "sig" sig

    it "redirects to 'return_sso_url' with 'sso' and 'sig' params" $ do
        testDB (createUser "bob" "barker")
        login "bob" "barker"

        let payload = encodePayload "nonce=foo&return_sso_url=bar"

        sig <- payloadSig payload

        request $ do
            setMethod "GET"
            setUrl DiscourseR
            addGetParam "sso" (decodeUtf8 payload)
            addGetParam "sig" sig

        withResponse $ \resp -> liftIO $ do
            simpleStatus resp `shouldBe` status303

            case lookup "location" (simpleHeaders resp) of
                Nothing -> expectationFailure "Missing location header"
                Just url -> do
                  let (path, query) = decodePath url
                  path `shouldBe` ["bar"]

                  lookup "sso" query `shouldSatisfy` isJust
                  lookup "sig" query `shouldSatisfy` isJust

    -- TODO: Test that payload and signature are correct

encodePayload :: ByteString -> ByteString
encodePayload = convertToBase Base64URLUnpadded

payloadSig :: ByteString -> YesodExample App Text
payloadSig payload = do
  secret <- appDiscourseSsoSecret . appSettings <$> getTestYesod
  pure (decodeUtf8 (convertToBase Base16
    ((Discourse.hmacSHA256 secret payload))))
