module Handler.Discourse (getDiscourseR) where

import Import

import Control.Lens hiding ((??))
import Control.Error ((??), hoistEither)
import Control.Monad.Trans.Except
import Database.Persist.Sql (fromSqlKey)

import qualified Web.Hashids as Hashids

import Avatar
import Discourse

-- Repurpose the discourse secret as the hashids salt
discourseUser :: DiscourseSecret -> UserId -> DiscourseUser
discourseUser (DiscourseSecret salt) =
    DiscourseUser . decodeUtf8 . Hashids.encode (Hashids.hashidsMinimum salt 6)
      . fromIntegral . fromSqlKey

-- | Respond to SSO requests from Discourse, authenticating users against our
-- database.
getDiscourseR :: Handler Html
getDiscourseR = do
    result <- runExceptT $ do
        -- Extract payload param
        mpayload <- lift $ fmap encodeUtf8 <$> lookupGetParam "sso"
        payload <- mpayload ?? NoPayload
        -- Extract sig param
        msig <- lift $ fmap encodeUtf8 <$> lookupGetParam "sig"
        sig <- msig ?? NoSignature
        -- Get SSO secret from settings
        secret <- lift $ getsYesod $ appDiscourseSsoSecret . appSettings
        -- Verify signature
        unless (validateSig secret payload sig) $ throwE InvalidSignature
        -- Extract nonce and return URL from payload
        dp <- hoistEither $ parsePayload payload
        -- Perform authentication and fetch user info
        Entity uid u <- lift requireAuth
        avatar <-
            lift $ getUserAvatar (StaticR img_default_avatar_png) (Just u)
        let uinfo = UserInfo
                { ssoEmail     = u ^. userEmail
                , ssoId        = discourseUser secret uid
                -- TODO no better option right now...
                , ssoUsername  = Nothing
                -- TODO no better option right now...
                , ssoFullName  = Nothing
                , ssoAvatarUrl = Just avatar
                -- TODO could link to Snowdrift user page
                , ssoBio       = Nothing
                }
        -- Compute new payload and sig
            uinfoPayload = userInfoPayload (dpNonce dp) uinfo
            uinfoSig = generateSig secret uinfoPayload
        -- Send them back to Discourse
        let params = [("sso", uinfoPayload), ("sig", uinfoSig)]
        return $ dpUrl dp <> decodeUtf8 (renderSimpleQuery True params)
    case result of
        Left err  -> invalidArgs [getDPErrorMsg err]
        Right url -> redirect url
