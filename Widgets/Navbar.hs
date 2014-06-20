
module Widgets.Navbar where

import Import

import Model.Currency
import Model.User
import Model.Project

import qualified Data.List as L

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

    (messages, edits, comments) <- case maybe_user of
        Nothing -> return ([], 0, 0)
        Just (Entity user_id user) -> handlerToWidget $ runDB $ do
            snowdrift_member <- isProjectAffiliated "snowdrift" user_id
            projects <- select $ from $ \ project -> return project

            messages :: [Entity Message] <- select $ from $ \ message -> do
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

            let unval (Value a) = a
                foldCounts (c1, c2) (c1', c2') =
                    (c1 + unval (L.head c1'), c2 + unval (L.head c2'))

            counts <- getCounts (Entity user_id user) projects

            let (comments, edits) = foldl foldCounts (0, 0) counts

            return (messages, edits, comments)

    $(widgetFile "navbar")
