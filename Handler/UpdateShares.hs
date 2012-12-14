module Handler.UpdateShares where

import Import

import Model.Currency

import Model.Shares
import Model.Project

import Database.Persist.GenericSql

import Widgets.Sidebar

confirmForm :: Int64 -> Form SharesPurchaseOrder
confirmForm shares = renderDivs $ SharesPurchaseOrder <$> areq hiddenField "" (Just shares)

getUpdateSharesR :: ProjectId -> Handler RepHtml
getUpdateSharesR project_id = do
    ((result, _), _) <- runFormGet $ buySharesForm 0
    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative
            user_id <- requireAuthId
            project <- runDB $ get404 project_id
            (confirm_form, _) <- generateFormPost $ confirmForm shares

            maybe_pledge <- runDB $ getBy $ UniquePledge user_id project_id

            defaultLayout $(widgetFile "update_shares")

        FormMissing -> defaultLayout [whamlet| form missing |]
        FormFailure _ -> defaultLayout [whamlet| form failure |]


postUpdateSharesR :: ProjectId -> Handler RepHtml
postUpdateSharesR project_id = do
    user_id <- requireAuthId
    ((result, _), _) <- runFormPost $ confirmForm 1

    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative

            success <- runDB $ do
                Just user <- get user_id
                Just account <- get $ userAccount user

                either_unique <- insertBy $ Pledge user_id project_id shares shares

                case either_unique of
                    Right _ -> return ()
                    Left (Entity pledge_id _) ->
                        if shares == 0
                         then delete pledge_id
                         else update pledge_id [ PledgeShares =. shares
                                               , PledgeFundedShares =. shares
                                               ]

                updateShareValue project_id

                user_pledges <- rawSql "SELECT ??, ?? FROM pledge JOIN project ON pledge.project = project.id WHERE pledge.\"user\" = ?;" [ unKey user_id ]

                let user_outlay = sum $ map (\ (Entity _ pledge, Entity _ project) -> projectShareValue project $* fromIntegral (pledgeShares pledge)) user_pledges :: Milray

                if accountBalance account < user_outlay $* 3
                 then do
                    rollback
                    return False
                 else return True

            if success
             then setMessage "you are now contributing to this project"
             else setMessage "you must have at least 3 months worth in your account to pledge additional shares"
            redirect $ ProjectR project_id

        _ -> do
            setMessage "error occurred in form submission"
            redirect $ UpdateSharesR project_id

