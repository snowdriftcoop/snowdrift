
module Widgets.Navbar where

import Import

import Model.Currency
import Model.User

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth

    alreadyExpired

    money_info <- case maybe_user of
        Nothing -> return Nothing
        Just (Entity user_id user) -> do
            (pledges, balance) <- handlerToWidget $ runDB $ do
                pledges :: [(Entity Project, Entity Pledge)] <- select $ from $
                    \ (project `InnerJoin` pledge) -> do
                        on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                        where_ $ pledge ^. PledgeUser ==. val user_id
                        return (project, pledge)

                Just account <- get (userAccount user)
                return (pledges, accountBalance account)

            let pledged = sum $ map (\ (project, pledge) ->
                    ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges

            return $ Just (balance, pledged)


    -- TODO: make stuff below generalize to project affiliates instead of snowdrift only

    messages <- case maybe_user of
        Nothing -> return []
        Just (Entity user_id user) -> handlerToWidget $ runDB $ do
            snowdrift_member <- isProjectAffiliated "snowdrift" user_id

            select $
             from $ \message -> do
             where_ $
                let
                    readSnowdriftMessages =
                        ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                            &&. isNothing (message ^. MessageTo))

                    readUserMessages =
                        ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                            &&. message ^. MessageTo ==. val (Just user_id))
                 in if snowdrift_member
                        then readSnowdriftMessages ||. readUserMessages
                        else readUserMessages
             return message

    $(widgetFile "navbar")
