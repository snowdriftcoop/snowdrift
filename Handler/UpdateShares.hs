module Handler.UpdateShares where

import Import

import Model.Currency

import Model.Shares
import Model.Project

import qualified Data.Text as T


confirmForm :: Int64 -> Form SharesPurchaseOrder
confirmForm shares = renderBootstrap3 $ SharesPurchaseOrder <$> areq' hiddenField "" (Just shares)

getUpdateSharesR :: Text -> Handler Html
getUpdateSharesR project_handle = do
    _ <- requireAuthId
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
    ((result, _), _) <- runFormGet $ pledgeForm project_id
    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative
            user_id <- requireAuthId
            (confirm_form, _) <- generateFormPost $ confirmForm shares

            maybe_pledge <- runDB $ getBy $ UniquePledge user_id project_id

            defaultLayout $ do
                setTitle . toHtml $ projectName project <> " - Change Your Contribution | Snowdrift.coop"
                $(widgetFile "update_shares")

        FormMissing -> defaultLayout [whamlet| form missing |]
        FormFailure _ -> defaultLayout [whamlet| form failure |]


postUpdateSharesR :: Text -> Handler Html
postUpdateSharesR project_handle = do
    ((result, _), _) <- runFormPost $ confirmForm 1

    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            -- TODO - refuse negative
            Just pledge_render_id <- fmap (read . T.unpack) <$> lookupSession pledgeRenderKey

            success <- runSYDB $ do
                Entity user_id user <- lift (lift requireAuth)
                Just account <- lift $ get (userAccount user)
                Entity project_id project <- lift $ getBy404 (UniqueProjectHandle project_handle)

                let user_outlay = projectShareValue project $* fromIntegral shares :: Milray
                if accountBalance account < user_outlay $* 3
                    then return False
                    else do
                        insertProjectPledgeDB user_id project_id shares pledge_render_id
                        lift (updateShareValue project_id)
                        return True

            if success
               then addAlert "success" "you are now pledged to support this project"
               else addAlert "warning"
                             "Sorry, you must have funds to support your pledge for at least 3 months at current share value. Please deposit additional funds to your account."

            redirect (ProjectR project_handle)

        _ -> do
            addAlert "danger" "error occurred in form submission"
            redirect $ UpdateSharesR project_handle
