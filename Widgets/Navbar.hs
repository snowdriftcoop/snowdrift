
module Widgets.Navbar where

import Import

import Model.Currency
import Model.User

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth

    alreadyExpired

    (money_info, num_unread_notifs, pledgeStat) <- case maybe_user of
        Nothing -> return (Nothing, 0, AllFunded)
        Just (Entity user_id user) -> do
            (pledges, balance, num_unread_notifs, pledgeStat) <-
                handlerToWidget $ runDB $ goDB user_id user

            let pledged = sum $ map (\(project, pledge) ->
                    ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges

            return (Just (balance, pledged), num_unread_notifs, pledgeStat)

    let hasUnderfunded = case pledgeStat of
            ExistsUnderfunded -> True
            AllFunded -> False

    $(widgetFile "navbar")

  where

    goDB user_id user = do
        pledges :: [(Entity Project, Entity Pledge)] <- select $ from $
            \(project `InnerJoin` pledge) -> do
                on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                where_ $ pledge ^. PledgeUser ==. val user_id
                return (project, pledge)
        Just account <- get (userAccount user)
        num_unread_notifs <- fetchNumUnreadNotificationsDB user_id
        pledgeStat <- pledgeStatus user_id
        return
            ( pledges
            , accountBalance account
            , num_unread_notifs
            , pledgeStat)
