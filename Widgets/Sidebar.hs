
module Widgets.Sidebar where

import Import

import Model.Role
import Model.Currency

import qualified Data.Set as S

import Database.Persist.Query.Join (selectOneMany, SelectOneMany (..))
import Database.Persist.Query.Join.Sql (runJoin)

sidebar :: Widget
sidebar = do
    maybe_user <- lift maybeAuth
    maybe_route <- lift getCurrentRoute

    lift alreadyExpired

    let role = fromMaybe Uninvited (userRole . entityVal <$> maybe_user)
        is_committee_member = role == CommitteeMember || role == Admin
        log_in_or_out =
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

    money_info <- case maybe_user of
        Nothing -> return Nothing
        Just (Entity user_id user) -> do
            (pledges, balance) <- lift $ runDB $ do
                pledges <- runJoin ((selectOneMany (PledgeProject <-.) pledgeProject) { somFilterMany = [ PledgeUser ==. user_id ] })
                Just account <- get (userAccount user)
                return (pledges, accountBalance account)
            let pledged = sum $ map (\ (project, pledge) -> maybe 0 ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) $ listToMaybe pledge) pledges
            return $ Just (balance, pledged)



    (messages, applications, edits, comments) <- case maybe_user of
        Nothing -> return mempty
        Just (Entity user_id user) -> lift $ runDB $ do
            messages :: [Entity Message] <- if is_committee_member
                         then selectList ([ MessageCreatedTs >=. userReadMessages user, MessageTo ==. Just user_id ] ||. [ MessageCreatedTs >=. userReadMessages user, MessageTo ==. Nothing ]) []
                         else selectList [ MessageCreatedTs >=. userReadMessages user, MessageTo ==. Just user_id ] []

            applications :: [Entity CommitteeApplication] <- if is_committee_member
                             then selectList [ CommitteeApplicationCreatedTs >=. userReadApplications user ] []
                             else return []

            edits :: [Entity WikiEdit] <- do
                edits <- selectList [ WikiEditTs >=. userReadEdits user, WikiEditUser !=. user_id ] []
                if null edits
                 then return []
                 else do
                    pages <- selectList [ WikiPageId <-. map (wikiEditPage . entityVal) edits ] []
                    let filtered_pages = map entityKey $ filter (\ (Entity _ page) -> userRole user >= wikiPageCanView page) pages
                    return $ filter (flip S.member (S.fromList filtered_pages) . wikiEditPage . entityVal) edits

            comments :: [Entity Comment] <- do
                comments <- selectList [ CommentCreatedTs >=. userReadComments user, CommentUser !=. user_id ] []
                if null comments
                 then return []
                 else do
                    pages <- selectList [ WikiPageId <-. map (commentPage . entityVal) comments ] []
                    let filtered_pages = map entityKey $ filter (\ (Entity _ page) -> userRole user >= wikiPageCanViewMeta page) pages
                    return $ filter (flip S.member (S.fromList filtered_pages) . commentPage . entityVal) comments

            return (messages, applications, edits, comments)

    $(widgetFile "sidebar")

