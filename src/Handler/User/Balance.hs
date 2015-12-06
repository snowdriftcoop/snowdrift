module Handler.User.Balance where

import Import

import qualified Data.Text as T

import Handler.Utils
import Model.Transaction
import Model.User
import View.Time
import View.User
import qualified Mechanism as Mech


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

    offset' <- lookupParamDefault "offset" 0
    limit' <- lookupParamDefault "count" 20

    (transactions, user_accounts, project_accounts) <-
        runDB $ Mech.userBalance user limit' offset'

    (add_funds_form, _) <- generateFormPost addTestCashForm

    defaultLayout $ do
        snowdriftDashTitle "User Balance" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_balance")
  where
    -- Warning: We can do better than 'read'.
    lookupParamDefault :: Text -> Int64 -> Handler Int64
    lookupParamDefault name def = do
        maybe_param <- lookup name . reqGetParams <$> getRequest
        return $ fromMaybe def $ do
            param_str <- maybe_param
            param <- listToMaybe $ reads $ T.unpack param_str
            return $ fst param

postUserBalanceR :: UserId -> Handler Html
postUserBalanceR user_id = do
    Entity viewer_id _ <- requireAuth
    unless (user_id == viewer_id) $
        permissionDenied "You can only add money to your own account."

    ((result, _), _) <- runFormPost addTestCashForm

    now <- liftIO getCurrentTime
    user <- runYDB $ get404 user_id

    case result of
        FormSuccess amount -> do
            if amount < 10
                then alertDanger "Sorry, minimum deposit is $10"
                else do
                    res <- runDB (Mech.incrementBalance user now amount)
                    either alertDanger
                           (const (alertSuccess "Balance updated"))
                           res
            redirect (UserBalanceR user_id)
        _ -> error "Error processing form."
