
module Widgets.Sidebar where

import Import

import Model.Role
import Model.Currency

import Data.Time.Calendar
import Data.Time.Clock

import Database.Persist.Query.Join (selectOneMany, SelectOneMany (..))
import Database.Persist.Query.Join.Sql (runJoin)

sidebar :: Widget
sidebar = do
    maybe_user <- lift maybeAuth
    maybe_route <- lift getCurrentRoute

    lift alreadyExpired

    let role = fromMaybe Uninvited (userRole . entityVal <$> maybe_user)
        log_in_or_out = do
            case maybe_user of
                Nothing ->
                    toWidget [hamlet|
                        <a href=@{AuthR LoginR}>Sign In
                    |]

                Just (Entity user_id user) -> do
                    let name = fromMaybe (userIdent user) $ userName user
                    toWidget [hamlet|
                        <a href=@{UserR user_id}>#{name}
                        <br>
                        <a href=@{AuthR LogoutR}>Sign Out
                    |]

        string :: String -> String
        string = id
        sb_link route name = do
            authorized <- lift $ isAuthorized route False
            case authorized of
             Authorized -> [whamlet|
                    $if maybe_route == Just route
                        #{string name}
                    $else
                        <a href=@{route}>
                            #{string name}
                    <br>
                |]

             _ -> return ()

        readMessages = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) . userReadMessages
        readApplications = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) . userReadApplications

    money_info <- case maybe_user of
        Nothing -> return Nothing
        Just (Entity user_id user) -> do
            (pledges, balance) <- lift $ runDB $ do
                pledges <- runJoin ((selectOneMany (PledgeProject <-.) pledgeProject) { somFilterMany = [ PledgeUser ==. user_id ] })
                Just account <- get (userAccount user)
                return (pledges, accountBalance account)
            let pledged = sum $ map (\ (project, pledge) -> maybe 0 ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) $ listToMaybe pledge) pledges
            return $ Just (balance, pledged)

    messages <- case maybe_user of
        Just (Entity user_id user) -> lift $ runDB $
            if role == CommitteeMember || role == Admin
             then selectList
                    (   [ MessageCreatedTs >=. readMessages user, MessageTo ==. Just user_id ]
                    ||. [ MessageCreatedTs >=. readMessages user, MessageTo ==. Nothing ]
                    ) []
             else selectList [ MessageCreatedTs >=. readMessages user, MessageTo ==. Just user_id ] []
        Nothing -> return []

    applications <- case maybe_user of
        Just (Entity _ user) -> lift $ runDB $
            if role == CommitteeMember || role == Admin
             then selectList [ CommitteeApplicationCreatedTs >=. readApplications user ] []
             else return []
        Nothing -> return []

    $(widgetFile "sidebar")

