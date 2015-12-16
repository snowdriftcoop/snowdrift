module Handler.User.VerifyEmail where

import Import

import Data.Maybe (fromJust)
import qualified Data.Maybe as Maybe

import Handler.User.Utils (checkEditUser)
import Model.User




-- /#UserId/verify-email/#Text

getUserVerifyEmailR :: UserId -> Text -> Handler Html
getUserVerifyEmailR user_id hash = do
    void $ checkEditUser user_id
    ver_uri <- getUrlRender <*> pure (UserVerifyEmailR user_id hash)
    (mver_email, muser_email) <- runDB $ (,)
        <$> fetchVerEmail ver_uri user_id
        <*> fetchUserEmail user_id
    if | Maybe.isNothing mver_email -> notFound
       | Maybe.isNothing muser_email -> do
             alertDanger $ "Failed to verify the email address since none is "
                        <> "associated with the account."
             redirect HomeR
       | otherwise -> do
             let ver_email  = fromJust mver_email
                 user_email = fromJust muser_email
             if ver_email == user_email
                 then do
                     runDB $ verifyEmailDB user_id
                     alertSuccess "Successfully verified the email address."
                     redirect HomeR
                 else do
                     alertDanger $ "Current email address does not match the "
                                <> "verification link."
                     redirect HomeR
