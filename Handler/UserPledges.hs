module Handler.UserPledges where

import Import

import Widgets.ProjectPledges


getOldUserPledgesR :: UserId -> Handler Html
getOldUserPledgesR = redirect . UserPledgesR

getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id =
    -- TODO: refine permissions here
    requireAuthId >> defaultLayout $(widgetFile "user_pledges")

