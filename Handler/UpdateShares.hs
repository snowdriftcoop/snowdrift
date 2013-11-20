module Handler.UpdateShares where

import Import

import Model.Currency

import Model.Shares
import Model.Project

import Widgets.Sidebar

confirmForm :: Int64 -> Form SharesPurchaseOrder
confirmForm shares = renderDivs $ SharesPurchaseOrder <$> areq hiddenField "" (Just shares)

getUpdateSharesR :: Text -> Handler Html
getUpdateSharesR project_handle = do
    _ <- requireAuthId

    ((result, _), _) <- runFormGet $ buySharesForm 0
    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative
            user_id <- requireAuthId
            Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle
            (confirm_form, _) <- generateFormPost $ confirmForm shares

            maybe_pledge <- runDB $ getBy $ UniquePledge user_id project_id

            defaultLayout $(widgetFile "update_shares")

        FormMissing -> defaultLayout [whamlet| form missing |]
        FormFailure _ -> defaultLayout [whamlet| form failure |]


postUpdateSharesR :: Text -> Handler Html
postUpdateSharesR project_handle = do
    user_id <- requireAuthId
    ((result, _), _) <- runFormPost $ confirmForm 1

    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative

            success <- runDB $ do
                Just user <- get user_id
                Just account <- get $ userAccount user
                Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle

                either_unique <- insertBy $ Pledge user_id project_id shares shares

                case either_unique of
                    Right _ -> return ()
                    Left (Entity pledge_id _) ->
                        if shares == 0
                         then delete $ from $ \ pledge -> do
                            where_ (pledge ^. PledgeId ==. val pledge_id)
                         else update $ \ pledge -> do
                            set pledge [ PledgeShares =. val shares
                                       , PledgeFundedShares =. val shares
                                       ]
                            where_ (pledge ^. PledgeId ==. val pledge_id)

                updateShareValue project_id

                user_pledges <- rawSql "SELECT ??, ?? FROM pledge JOIN project ON pledge.project = project.id WHERE pledge.\"user\" = ?;" [ unKey user_id ]

                let user_outlay = sum $ map (\ (Entity _ pledge, Entity _ project) -> projectShareValue project $* fromIntegral (pledgeShares pledge)) user_pledges :: Milray

                if accountBalance account < user_outlay $* 3
                 then do
                    transactionUndo
                    return False
                 else return True

            setMessage $ if success
             then "you are now pledged to support this project"
             else "you must have at least 3 months worth in your account to pledge additional shares"
            redirect $ ProjectR project_handle

        _ -> do
            setMessage "error occurred in form submission"
            redirect $ UpdateSharesR project_handle

