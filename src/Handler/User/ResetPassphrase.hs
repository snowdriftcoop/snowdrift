module Handler.User.ResetPassphrase where

import Import

import Handler.Utils
import Handler.User.Utils (resetPassphrase)
import Model.User
import View.User
-- /#UserId/reset-passphrase/#Text

checkResetPassphrase :: UserId -> Text -> Handler User
checkResetPassphrase user_id hash = do
    uri    <- getUrlRender <*> pure (UserResetPassphraseR user_id hash)
    memail <- runDB $ fetchUserEmail user_id
    case memail of
        Nothing    -> notFound
        Just email ->
            runYDB $ do
                -- Check whether the hash is in the DB.
                void $ getBy404 $ UniquePassphraseReset user_id email uri
                get404 user_id

getUserResetPassphraseR :: UserId -> Text -> Handler Html
getUserResetPassphraseR user_id hash = do
    user <- checkResetPassphrase user_id hash
    (form, enctype) <- generateFormPost setPassphraseForm
    defaultLayout $ do
        snowdriftDashTitle "Set Passphrase" $
            userDisplayName (Entity user_id user)
        $(widgetFile "set-passphrase")

postUserResetPassphraseR :: UserId -> Text -> Handler Html
postUserResetPassphraseR user_id hash = do
    user <- checkResetPassphrase user_id hash
    ((result, form), enctype) <- runFormPost setPassphraseForm
    case result of
        FormSuccess SetPassphrase {..} ->
            resetPassphrase user_id user passphrase passphrase' $
                UserResetPassphraseR user_id hash
        _ -> do
            alertDanger "Oops, failed to set the passphrase."
            defaultLayout $(widgetFile "set-passphrase")
