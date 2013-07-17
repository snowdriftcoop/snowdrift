module Handler.UserPledges where

import Import

import Widgets.ProjectPledges
import Widgets.Sidebar

getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id = defaultLayout $(widgetFile "user_pledges")

