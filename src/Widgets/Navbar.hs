
module Widgets.Navbar where

import Import

import Model.User

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth
    alreadyExpired

    num_unread_notifs <- case maybe_user of
        Nothing -> return 0
        Just (Entity user_id _) ->
            handlerToWidget
                (runDB (fetchNumUnreadNotificationsDB  user_id))
    $(widgetFile "navbar")
