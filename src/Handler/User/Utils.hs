module Handler.User.Utils
    ( checkEditUser
    , resetPassword
    , startEmailVerification
    ) where

import Import

import Yesod.Auth.HashDB (setPassword)
import Model.ResetPassphrase (deleteFromResetPassword)

import Model.User
import Handler.Utils (newHash)


checkEditUser :: UserId -> Handler UserId
checkEditUser user_id = do
    viewer_id <- requireAuthId
    unless
        (user_id == viewer_id)
        (permissionDenied "You can only modify your own profile.")
    return viewer_id

resetPassword :: RedirectUrl App route
              => UserId -> User -> Text -> Text -> route -> Handler Html
resetPassword user_id user password password' route =
    if password == password'
        then do
            user' <- setPassword password user
            runDB $ do
                updateUserPasswordDB user_id (userHash user') (userSalt user')
                deleteFromResetPassword user_id
            alertSuccess "You successfully updated your passphrase."
            redirect $ UserR user_id
        else do
            alertDanger "The passphrases you entered do not match."
            redirect route

startEmailVerification :: UserId -> Text -> HandlerT App IO ()
startEmailVerification user_id user_email = do
    hash    <- liftIO newHash
    ver_uri <- getUrlRender <*> pure (UserVerifyEmailR user_id hash)
    runDB $ do
        insert_ $ EmailVerification user_id user_email ver_uri False
        update $ \u -> do
            set u [UserEmail_verified =. val False]
            where_ $ u ^. UserId ==. val user_id
    alertSuccess $ "Verification email has been sent to " <> user_email <> "."
