module Handler.User.Tickets where

import Import

import Handler.Utils
import Model.User

getUserTicketsR :: UserId -> Handler Html
getUserTicketsR user_id = do
    user <- runYDB $ get404 user_id
    mviewer_id <- maybeAuthId
    claimed_tickets <- claimedTickets user_id
    watched_tickets <- watchedTickets user_id

    defaultLayout $ do
        snowdriftDashTitle "User Tickets" $
            userDisplayName (Entity user_id user)

        $(widgetFile "user_tickets")
