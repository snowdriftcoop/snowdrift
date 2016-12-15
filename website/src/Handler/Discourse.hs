module Handler.Discourse (getDiscourseR, getDiscourseRedirectR) where

import Import
import Control.Lens
import Control.Monad.Trans.Except
import Avatar
import Discourse
import qualified Data.Text as T

getDiscourseR :: Handler Html
getDiscourseR = do
    result <- runExceptT $ do
        -- Extract payload param
        mpayload <- lift $ fmap encodeUtf8 <$> lookupGetParam "sso"
        payload <- maybe (throwE "No payload") return mpayload
        -- Extract sig param
        msig <- lift $ fmap encodeUtf8 <$> lookupGetParam "sig"
        sig <- maybe (throwE "No sig") return msig
        -- Get SSO secret from settings
        secret <- lift $ getsYesod $ appDiscourseSsoSecret . appSettings
        -- Verify signature
        unless (validateSig secret payload sig) $ throwE "Signature is invalid"
        -- Extract nonce and return URL from payload
        (nonce, baseUrl) <- case parsePayload payload of
            Left err -> throwE $ "Payload invalid: " <> pack err
            Right p  -> return p
        -- Perform authentication and fetch user info
        Entity uid u <- lift requireAuth
        avatar <-
            lift $ getUserAvatar (StaticR img_default_avatar_png) (Just u)
        let uinfo = UserInfo
                { ssoEmail     = u ^. userEmail
                , ssoId        = uid
                -- TODO no better option right now...
                , ssoUsername  = Nothing
                -- TODO no better option right now...
                , ssoFullName  = Nothing
                , ssoAvatarUrl = Just avatar
                -- TODO could link to Snowdrift user page
                , ssoBio       = Nothing
                }
        -- Compute new payload and sig
            uinfoPayload = userInfoPayload nonce uinfo
            uinfoSig = generateSig secret uinfoPayload
        -- Send them back to Discourse
        let params = [("sso", uinfoPayload), ("sig", uinfoSig)]
        return $ baseUrl <> decodeUtf8 (renderSimpleQuery True params)
    case result of
        Left err  -> invalidArgs [err]
        Right url -> redirect url


getDiscourseRedirectR :: Handler Html
getDiscourseRedirectR = do
    muser <- maybeAuth
    durl <- getsYesod $ appDiscourseRootUrl . appSettings
    redirect $ T.append durl $ maybe T.empty addSSO muser

    where
        addSSO :: a -> T.Text
        addSSO _ = T.pack "/session/sso"
