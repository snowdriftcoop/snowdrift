module Handler.User.ResetPassword where

import Import

import Handler.Utils
import Handler.User.Utils (resetPassword)
import Model.User
import View.User
-- /#UserId/reset-password/#Text

checkResetPassword :: UserId -> Text -> Handler User
checkResetPassword user_id hash = do
    uri    <- getUrlRender <*> pure (UserResetPasswordR user_id hash)
    memail <- runDB $ fetchUserEmail user_id
    case memail of
        Nothing    -> notFound
        Just email ->
            runYDB $ do
                -- Check whether the hash is in the DB.
                void $ getBy404 $ UniquePasswordReset user_id email uri
                get404 user_id

getUserResetPasswordR :: UserId -> Text -> Handler Html
getUserResetPasswordR user_id hash = do
    user <- checkResetPassword user_id hash
    (form, enctype) <- generateFormPost setPasswordForm
    defaultLayout $ do
        snowdriftDashTitle "Set Passphrase" $
            userDisplayName (Entity user_id user)
        $(widgetFile "set_password")

postUserResetPasswordR :: UserId -> Text -> Handler Html
postUserResetPasswordR user_id hash = do
    user <- checkResetPassword user_id hash
    ((result, form), enctype) <- runFormPost setPasswordForm
    case result of
        FormSuccess SetPassword {..} ->
            resetPassword user_id user password password' $
                UserResetPasswordR user_id hash
        _ -> do
            alertDanger "Oops, failed to set the passphrase."
            defaultLayout $(widgetFile "set_password")
