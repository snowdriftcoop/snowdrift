module Handler.User.Pledges where

import Import

import Handler.Utils
import Model.User
import Widgets.UserPledges

-- /#UserId/pledges

getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id = do
    -- TODO: refine permissions here
    _ <- requireAuthId
    user <- runYDB $ get404 user_id

    defaultLayout $ do
        snowdriftDashTitle "User Pledges" $
            userDisplayName (Entity user_id user)

        $(widgetFile "user_pledges")
