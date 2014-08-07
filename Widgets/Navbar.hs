
module Widgets.Navbar where

import Import

import Model.Currency
import Model.User

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth

    alreadyExpired

    (money_info, num_unread_messages) <- case maybe_user of
        Nothing -> return (Nothing, 0)
        Just (Entity user_id user) -> do
            (pledges, balance, num_unread_messages) <- handlerToWidget $ runDB $ do
                pledges :: [(Entity Project, Entity Pledge)] <- select $ from $
                    \ (project `InnerJoin` pledge) -> do
                        on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                        where_ $ pledge ^. PledgeUser ==. val user_id
                        return (project, pledge)
                Just account <- get (userAccount user)
                num_unread_messages <- fetchNumUnreadMessagesDB user_id
                return (pledges, accountBalance account, num_unread_messages)

            let pledged = sum $ map (\ (project, pledge) ->
                    ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges

            return $ (Just (balance, pledged), num_unread_messages)

    $(widgetFile "navbar")
