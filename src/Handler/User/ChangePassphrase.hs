module Handler.User.ChangePassphrase where

import Import

import Yesod.Auth.HashDB (validateUser)

import Handler.Utils
import Handler.User.Utils
import Model.User
import View.User

getUserChangePasswordR :: UserId -> Handler Html
getUserChangePasswordR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    (form, enctype) <- generateFormPost changePasswordForm
    defaultLayout $ do
        snowdriftDashTitle "Change Passphrase" $
            userDisplayName (Entity user_id user)
        $(widgetFile "change-passphrase")

postUserChangePasswordR :: UserId -> Handler Html
postUserChangePasswordR user_id = do
    void $ checkEditUser user_id
    ((result, form), enctype) <- runFormPost changePasswordForm
    case result of
        FormSuccess ChangePassword {..} -> do
            user <- runYDB $ get404 user_id
            is_valid_password <- validateUser (UniqueUser $ userIdent user)
                                     currentPassword
            if is_valid_password
                then resetPassword user_id user newPassword newPassword' $
                         UserChangePasswordR user_id
                else do
                    alertDanger "Sorry, that is not the correct current passphrase."
                    defaultLayout $(widgetFile "change-passphrase")
        _ -> do
            alertDanger "Oops, failed to update the passphrase."
            defaultLayout $(widgetFile "change-passphrase")
