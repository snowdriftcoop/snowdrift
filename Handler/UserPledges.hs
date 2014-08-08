module Handler.UserPledges where

import Import

import Model.User

import Widgets.ProjectPledges


getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id = do
    -- TODO: refine permissions here
    _ <- requireAuthId
    user <- runYDB $ get404 user_id
    defaultLayout $ do
        setTitle . toHtml $
            "User Pledges - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"

        $(widgetFile "user_pledges")

