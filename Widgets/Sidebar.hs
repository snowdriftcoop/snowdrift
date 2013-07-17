
module Widgets.Sidebar where

import Import

import Model.Role
import Model.Currency

import qualified Data.Set as S

sidebar :: Widget
sidebar = do
    maybe_user <- handlerToWidget maybeAuth
    maybe_route <- handlerToWidget getCurrentRoute

    alreadyExpired

    let role = fromMaybe Uninvited (userRole . entityVal <$> maybe_user)
        is_committee_member = role == CommitteeMember || role == Admin
        log_in_or_out =
            case maybe_user of
                Nothing ->
                    toWidget [hamlet|
                        <a href=@{AuthR LoginR}>Sign In / Create Account
                    |] :: Widget

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
            authorized <- handlerToWidget $ isAuthorized route False
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
            (pledges, balance) <- handlerToWidget $ runDB $ do
                pledges :: [(Entity Project, Entity Pledge)] <- select $ from $ \ (project `InnerJoin` pledge) -> do
                    on (pledge ^. PledgeProject ==. project ^. ProjectId)
                    where_ (pledge ^. PledgeUser ==. val user_id)
                    return (project, pledge)

                Just account <- get (userAccount user)
                return (pledges, accountBalance account)
            let pledged = sum $ map (\ (project, pledge) -> ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges
            return $ Just (balance, pledged)



    (messages, applications, edits, comments) <- case maybe_user of
        Nothing -> return mempty
        Just (Entity user_id user) -> handlerToWidget $ runDB $ do
            messages :: [Entity Message] <- select $ from $ \ message -> do
                            where_ $
                                ( if is_committee_member
                                    then (||.
                                            ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                                              &&. message ^. MessageTo ==. val Nothing )
                                         )
                                    else id
                                ) $ ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                                        &&. message ^. MessageTo ==. val (Just user_id)
                                    )

                            return message


            applications :: [Entity CommitteeApplication] <- if is_committee_member
                             then select $ from $ \ application -> do
                                where_ ( application ^. CommitteeApplicationCreatedTs >=. val (userReadApplications user) )
                                return application
                             else return []

            edits :: [Entity WikiEdit] <- do
                edits <- select $ from $ \ wiki_edit -> do
                    where_ ( wiki_edit ^. WikiEditTs >=. val (userReadEdits user) &&. wiki_edit ^. WikiEditUser !=. val user_id )
                    return wiki_edit

                if null edits
                 then return []
                 else do
                    pages <- select $ from $ \ wiki_page -> do
                        where_ ( wiki_page ^. WikiPageId `in_` valList (map (wikiEditPage . entityVal) edits) )
                        return wiki_page

                    let filtered_pages = map entityKey $ filter (\ (Entity _ page) -> userRole user >= wikiPageCanViewMeta page) pages
                    return $ filter (flip S.member (S.fromList filtered_pages) . wikiEditPage . entityVal) edits

            comments :: [Entity Comment] <- do
                comments <- select $ from $ \ comment -> do
                    where_ ( comment ^. CommentCreatedTs >=. val (userReadComments user) &&. comment ^. CommentUser !=. val user_id )
                    return comment
                if null comments
                 then return []
                 else do
                    pages <- select $ from $ \ wiki_page -> do
                        where_ ( wiki_page ^. WikiPageId `in_` valList (map (commentPage . entityVal) comments) )
                        return wiki_page

                    let filtered_pages = map entityKey $ filter (\ (Entity _ page) -> userRole user >= wikiPageCanViewMeta page) pages
                    return $ filter (flip S.member (S.fromList filtered_pages) . commentPage . entityVal) comments

            return (messages, applications, edits, comments)

    $(widgetFile "sidebar")

