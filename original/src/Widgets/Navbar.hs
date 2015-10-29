
module Widgets.Navbar where

import Import

import Mechanism
import Model.User

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth

    alreadyExpired

    money_info <- handlerToWidget (runDB (moneyInfo maybe_user))
    (num_unread_notifs, pledgeStat) <- case maybe_user of
        Nothing -> return (0, AllFunded)
        Just (Entity user_id _) -> do
            (num_unread_notifs, pledgeStat) <-
                handlerToWidget $ runDB $ goDB user_id

            return (num_unread_notifs, pledgeStat)

    let hasUnderfunded = case pledgeStat of
            ExistsUnderfunded -> True
            AllFunded -> False

    $(widgetFile "navbar")

  where

    goDB user_id = do
        num_unread_notifs <- fetchNumUnreadNotificationsDB user_id
        pledgeStat <- pledgeStatus user_id
        return
            ( num_unread_notifs
            , pledgeStat)
