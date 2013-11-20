module Handler.UserBalance where

import Import

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import Model.Transaction
import Model.Currency
import Model.User

import Widgets.Sidebar
import Widgets.Time

import Data.Maybe (catMaybes)

lookupParamDefault :: Read a => Text -> a -> Handler a
lookupParamDefault name def = do
    maybe_param <- lookup name <$> reqGetParams <$> getRequest
    return $ fromMaybe def $ do
        param_str <- maybe_param
        param <- listToMaybe $ reads $ T.unpack param_str
        return $ fst param
        

getUserBalanceR :: UserId -> Handler Html
getUserBalanceR user_id = do
    Entity viewer_id viewer <- requireAuth
    user <- runDB $ get404 user_id

    -- TODO: restrict viewing balance to user or snowdrift admins (logged) before moving to real money
    -- when (user_id /= viewer_id) $ permissionDenied "You can only view your own account balance history."

    Just account <- runDB $ get $ userAccount user

    offset' <- lookupParamDefault "offset" 0
    limit' <- lookupParamDefault "count" 20

    (transactions, user_accounts, project_accounts) <- runDB $ do
        transactions <- select $ from $ \ transaction -> do
            where_ ( transaction ^. TransactionCredit ==. val (Just (userAccount user))
                    ||. transaction ^. TransactionDebit ==. val (Just (userAccount user)))
            orderBy [ desc (transaction ^. TransactionTs) ]
            limit limit'
            offset offset'
            return transaction

        let accounts = catMaybes $ S.toList $ S.fromList $ map (transactionCredit . entityVal) transactions ++ map (transactionDebit . entityVal) transactions 

        users <- selectList [ UserAccount <-. accounts ] []
        projects <- selectList [ ProjectAccount <-. accounts ] []

        let mkMapBy :: Ord b => (a -> b) -> [a] -> M.Map b a
            mkMapBy f = M.fromList . map (\ e -> (f e, e))

        return
            ( transactions
            , mkMapBy (userAccount . entityVal) users
            , mkMapBy (projectAccount . entityVal) projects
            )
        
    (add_funds_form, _) <- generateFormPost addTestCashForm

    defaultLayout $(widgetFile "user_balance")



postUserBalanceR :: UserId -> Handler Html
postUserBalanceR user_id = do
    Entity viewer_id _ <- requireAuth
    user <- runDB $ get404 user_id

    when (user_id /= viewer_id) $ runDB $ do
        is_admin <- isProjectAdmin "snowdrift" viewer_id
        when (not $ is_admin) $ lift $ permissionDenied "You can only add money to your own account."

    ((result, _), _) <- runFormPost addTestCashForm

    now <- liftIO getCurrentTime

    case result of
        FormSuccess amount -> do
            if amount < 10
             then setMessage "Must load money in increments of at least $10."
             else do
                runDB $ do
                    _ <- insert $ Transaction now (Just $ userAccount user) Nothing amount "Test Load" Nothing
                    update $ \ account -> do
                        set account [ AccountBalance +=. val amount ]
                        where_ ( account ^. AccountId ==. val (userAccount user) )

                setMessage "Balance updated."
            redirect $ UserBalanceR user_id

        _ -> error "Error processing form."


addTestCashForm :: Form Milray
addTestCashForm = renderDivs $ fromInteger . (10000 *) <$> areq intField "Add (fake) money to your account (in whole dollars)" (Just 10)
