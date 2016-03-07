module Handler.User.Balance where

import Import

import Handler.Utils
import Model.User
import View.User

-- check permissions for user balance view
getUserBalanceR :: UserId -> Handler Html
getUserBalanceR user_id = do
    viewer_id <- requireAuthId
    if viewer_id /= user_id
        then permissionDenied "You must be a site administrator to view user balances."
        else getUserBalanceR' user_id

getUserBalanceR' :: UserId -> Handler Html
getUserBalanceR' user_id = do
    user <- runYDB $ get404 user_id

    -- TODO: restrict viewing balance to user or snowdrift admins (logged)
    -- before moving to real money
    -- when (user_id /= viewer_id)
    --      (permissionDenied
    --         "You can only view your own account balance history.")

    Just account <- runDB $ get $ userAccount user

    (add_funds_form, _) <- generateFormPost addTestCashForm

    defaultLayout $ do
        snowdriftDashTitle "User Balance" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_balance")

postUserBalanceR :: UserId -> Handler Html
postUserBalanceR _user_id = error "Unhandled"
