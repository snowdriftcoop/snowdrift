{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import           Data.Filter
import           Data.Order
import           Model.Application
import           Model.Discussion
import           Model.Issue
import           Model.Markdown
import           Model.Markdown.Diff
import           Model.Project
import           Model.Role
import           Model.SnowdriftEvent
import           Model.User
import           Model.Wiki
import           View.PledgeButton
import           View.Project
import           View.SnowdriftEvent
import           Widgets.Preview
import           Widgets.Time

import           Data.List            (sortBy)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, maybeToList)
import qualified Data.Set             as S
import qualified Data.Text            as T
import           System.Random        (randomIO)
import           Text.Printf


--------------------------------------------------------------------------------
-- Utility functions

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def = do
    maybe_value <- lookupGetParam name
    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack

-- | Require any of the given Roles, failing with permissionDenied if none are satisfied.
requireRolesAny :: [Role] -> Text -> Text -> Handler (UserId, Entity Project)
requireRolesAny roles project_handle err_msg = do
    user_id <- requireAuthId

    (project, ok) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)

        ok <- userHasRolesAnyDB roles user_id project_id

        return (project, ok)

    unless ok $
        permissionDenied err_msg

    return (user_id, project)

-------------------------------------------------------------------------------
--

getProjectsR :: Handler Html
getProjectsR = do
    projects <- runDB fetchAllProjectsDB
    defaultLayout $ do
        setTitle "Projects | Snowdrift.coop"
        $(widgetFile "projects")

--------------------------------------------------------------------------------
-- /

getProjectR :: Text -> Handler Html
getProjectR project_handle = do
    mviewer_id <- maybeAuthId

    (project_id, project, pledges, pledge) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- getProjectShares project_id
        pledge <- case mviewer_id of
            Nothing -> return Nothing
            Just viewer_id -> getBy $ UniquePledge viewer_id project_id

        return (project_id, project, pledges, pledge)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        renderProject (Just project_id) project pledges pledge

postProjectR :: Text -> Handler Html
postProjectR project_handle = do
    (viewer_id, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description tags github_repo) -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "update"
            case mode of
                Just "preview" -> do
                    let preview_project = project { projectName = name, projectDescription = description, projectGithubRepo = github_repo }

                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))
                    defaultLayout $ previewWidget form action $ renderProject (Just project_id) preview_project [] Nothing

                Just x | x == action -> do
                    runDB $ do
                        when (projectDescription project /= description) $ do
                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update
                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update

                        update $ \ p -> do
                            set p [ ProjectName =. val name, ProjectDescription =. val description, ProjectGithubRepo =. val github_repo ]
                            where_ (p ^. ProjectId ==. val project_id)

                        tag_ids <- forM tags $ \ tag_name -> do
                            tag_entity_list <- select $ from $ \ tag -> do
                                where_ (tag ^. TagName ==. val tag_name)
                                return tag

                            case tag_entity_list of
                                [] -> insert $ Tag tag_name
                                Entity tag_id _ : _ -> return tag_id


                        delete $ from $ \ project_tag -> where_ (project_tag ^. ProjectTagProject ==. val project_id)

                        forM_ tag_ids $ \ tag_id -> insert $ ProjectTag project_id tag_id

                    alertSuccess "project updated"
                    redirect $ ProjectR project_handle

                _ -> do
                    addAlertEm "danger" "unrecognized mode" "Error: "
                    redirect $ ProjectR project_handle
        x -> do
            alertDanger (T.pack $ show x)
            redirect (ProjectR project_handle)

--------------------------------------------------------------------------------
-- /application

getApplicationsR :: Text -> Handler Html
getApplicationsR project_handle = do
    viewer_id <- requireAuthId

    (project, applications) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        applications <- fetchProjectVolunteerApplicationsDB project_id
        userReadVolunteerApplicationsDB viewer_id
        return (project, applications)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Volunteer Applications | Snowdrift.coop"
        $(widgetFile "applications")

getApplicationR :: Text -> VolunteerApplicationId -> Handler Html
getApplicationR project_handle application_id = do
    viewer_id <- requireAuthId
    (project, user, application, interests, num_interests) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        application <- get404 application_id
        let user_id = volunteerApplicationUser application
        user <- get404 user_id
        (interests, num_interests) <- (T.intercalate ", " &&& length) <$> fetchApplicationVolunteerInterestsDB application_id
        return (project, Entity user_id user, application, interests, num_interests)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Volunteer Application - " <> userDisplayName user <> " | Snowdrift.coop"
        $(widgetFile "application")

--------------------------------------------------------------------------------
-- /button.png

getProjectPledgeButtonR :: Text -> Handler TypedContent
getProjectPledgeButtonR project_handle = do
   pledges <- runYDB $ do
        Entity project_id _project <- getBy404 $ UniqueProjectHandle project_handle
        getProjectShares project_id
   let png = overlayImage blankPledgeButton $
        fillInPledgeCount (fromIntegral (length pledges))
   respond "image/png" png

--------------------------------------------------------------------------------
-- /b

getProjectBlogR :: Text -> Handler Html
getProjectBlogR project_handle = do
    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"
    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    let apply_offset blog = maybe id (\ from_blog rest -> blog ^. ProjectBlogId >=. val from_blog &&. rest) maybe_from

    (posts, next) <- fmap (splitAt post_count) $ runDB $
        select $
        from $ \blog -> do
        where_ $ apply_offset blog $ blog ^. ProjectBlogProject ==. val project_id
        orderBy [ desc $ blog ^. ProjectBlogTime, desc $ blog ^. ProjectBlogId ]
        limit (fromIntegral post_count + 1)
        return blog

    renderRouteParams <- getUrlRenderParams

    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "project_blog")


getNewProjectBlogPostR :: Text -> Handler Html
getNewProjectBlogPostR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    (blog_form, _) <- generateFormPost $ projectBlogForm Nothing

    defaultLayout $ do
        setTitle . toHtml $ "Post To " <> projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "new_blog_post")


postNewProjectBlogPostR :: Text -> Handler Html
postNewProjectBlogPostR project_handle = do
    (viewer_id, Entity project_id _) <-
        requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ projectBlogForm Nothing

    case result of
        FormSuccess mk_blog_post -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "post"
            case mode of
                Just "preview" -> do
                    let blog_post :: ProjectBlog
                        blog_post = mk_blog_post now viewer_id project_id (Key $ PersistInt64 0)
                        title = projectBlogTitle blog_post
                        handle = projectBlogHandle blog_post
                        top_content = projectBlogTopContent blog_post
                        bottom_content = fromMaybe "" $ projectBlogBottomContent blog_post
                        content = top_content <> bottom_content

                    (form, _) <- generateFormPost $ projectBlogForm $ Just (title, handle, content)

                    defaultLayout $ previewWidget form action $ renderBlogPost project_handle blog_post

                Just x | x == action -> do
                    void $ runDB $ do
                        discussion_id <- insert $ Discussion 0

                        let blog_post :: ProjectBlog
                            blog_post = mk_blog_post now viewer_id project_id discussion_id

                        insert blog_post

                    alertSuccess "posted"
                    redirect $ ProjectBlogR project_handle

                x -> do
                    addAlertEm "danger" ("unrecognized mode: " <> T.pack (show x)) "Error: "
                    redirect $ NewProjectBlogPostR project_handle

        x -> do
            alertDanger $ T.pack $ show x
            redirect $ NewProjectBlogPostR project_handle


getProjectBlogPostR :: Text -> Text -> Handler Html
getProjectBlogPostR project_handle blog_post_handle = do
    (project, blog_post) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ blog_post <- getBy404 $ UniqueProjectBlogPost project_id blog_post_handle

        return (project, blog_post)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog - " <> projectBlogTitle blog_post <> " | Snowdrift.coop"

        renderBlogPost project_handle blog_post

--------------------------------------------------------------------------------
-- /edit

getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    (_, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    tags <- runDB $
        select $
        from $ \ (p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        $(widgetFile "edit_project")

--------------------------------------------------------------------------------
-- /feed

-- | This function is responsible for hitting every relevant event table. Nothing
-- statically guarantees that.
getProjectFeedR :: Text -> Handler Html
getProjectFeedR project_handle = do
    muser_id <- maybeAuthId
    before <- maybe (liftIO getCurrentTime) (return . read . T.unpack) =<< lookupGetParam "before"
    (events, discussion_wiki_pages_map, wiki_pages_map, users_map) <- runYDB $ do
        Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)

        comment_posted_entities  <- fetchProjectCommentsPostedOnWikiPagesBeforeDB project_id muser_id before
        comment_pending_entities <- fetchProjectCommentsPendingBeforeDB project_id muser_id before
        wiki_page_entities       <- fetchProjectWikiPagesBeforeDB project_id before
        wiki_edit_entities       <- fetchProjectWikiEditsBeforeDB project_id before
        new_pledge_entities      <- fetchProjectNewPledgesBeforeDB project_id before
        updated_pledges          <- fetchProjectUpdatedPledgesBeforeDB project_id before
        deleted_pledge_events    <- fetchProjectDeletedPledgesBeforeDB project_id before

        -- Suplementary maps for displaying the data. If something above requires extra
        -- data to display the project feed row, it MUST be used to fetch the data below!
        -- The Maybes from Data.Map.lookup are unsafely STRIPPED in the views!

        let comments       = map entityVal (comment_posted_entities <> comment_pending_entities)
            wiki_edits     = map entityVal wiki_edit_entities
            shares_pledged = map entityVal (new_pledge_entities <> (map snd updated_pledges))

        -- WikiPages that can be keyed by a Comment's DiscussionId (i.e. the Comment *is* on a WikiPage)
        discussion_wiki_pages_map <- M.fromList . map (\e@(Entity _ WikiPage{..}) -> (wikiPageDiscussion, e)) <$>
                                       fetchDiscussionWikiPagesInDB (map commentDiscussion comments)

        -- WikiPages keyed by their own IDs (contained in a WikiEdit)
        wiki_pages_map <- entitiesMap <$> fetchWikiPagesInDB (map wikiEditPage wiki_edits)

        -- All users: Comment posters, WikiPage creators, WikiEdit makers,
        -- and Pledgers (new, updated, and deleted).
        users_map <- (\a b c d -> a <> b <> c <> d)
            <$> (entitiesMap <$> fetchUsersInDB (map commentUser comments))
            <*> (entitiesMap <$> fetchUsersInDB (map wikiEditUser wiki_edits))
            <*> (entitiesMap <$> fetchUsersInDB (map sharesPledgedUser shares_pledged))
            <*> (entitiesMap <$> fetchUsersInDB (map (\(EventDeletedPledge _ user_id _ _) -> user_id) deleted_pledge_events))

        let events = sortBy snowdriftEventNewestToOldest . mconcat $
              [ map (onEntity ECommentPosted)  comment_posted_entities
              , map (onEntity ECommentPending) comment_pending_entities
              , map (onEntity EWikiPage)       wiki_page_entities
              , map (onEntity EWikiEdit)       wiki_edit_entities
              , map (onEntity ENewPledge)      new_pledge_entities
              , map eup2se                     updated_pledges
              , map edp2se                     deleted_pledge_events
              ]
        return (events, discussion_wiki_pages_map, wiki_pages_map, users_map)
    defaultLayout $(widgetFile "project_feed")
  where
    -- "event updated pledge to snowdrift event". Makes above code cleaner.
    eup2se :: (Int64, Entity SharesPledged) -> SnowdriftEvent
    eup2se (old_shares, Entity shares_pledged_id shares_pledged) = EUpdatedPledge old_shares shares_pledged_id shares_pledged

    -- "event deleted pledge to snowdrift event". Makes above code cleaner.
    edp2se :: EventDeletedPledge -> SnowdriftEvent
    edp2se (EventDeletedPledge a b c d) = EDeletedPledge a b c d

--------------------------------------------------------------------------------
-- /invite

getInviteR :: Text -> Handler Html
getInviteR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . T.unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR project_handle <$> maybe_invite_code
    (invite_form, _) <- generateFormPost inviteForm

    outstanding_invites <- runDB $
        select $
        from $ \ invite -> do
        where_ ( invite ^. InviteRedeemed ==. val False )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        return invite

    redeemed_invites <- runDB $
        select $
        from $ \ invite -> do
        where_ ( invite ^. InviteRedeemed ==. val True )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        limit 20
        return invite

    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites
        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites
        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites
        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters

    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []

    let users = M.fromList $ map (entityKey &&& id) user_entities

    let format_user Nothing = "NULL"
        format_user (Just user_id) =
            let Entity _ user = users M.! user_id
             in fromMaybe (userIdent user) $ userName user

        format_inviter user_id =
            userDisplayName $ users M.! user_id

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - Send Invite | Snowdrift.coop"
        $(widgetFile "invite")

postInviteR :: Text -> Handler Html
postInviteR project_handle = do
    (user_id, Entity project_id _) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    invite <- liftIO randomIO

    ((result, _), _) <- runFormPost inviteForm
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = T.pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (T.pack $ show role)

        _ -> alertDanger "Error in submitting form."

    redirect $ InviteR project_handle

--------------------------------------------------------------------------------
-- /patrons

getProjectPatronsR :: Text -> Handler Html
getProjectPatronsR project_handle = do
    _ <- requireAuthId

    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges, user_payouts_map) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- select $ from $ \ (pledge `InnerJoin` user) -> do
            on_ $ pledge ^. PledgeUser ==. user ^. UserId
            where_ $ pledge ^. PledgeProject ==. val project_id
                &&. pledge ^. PledgeFundedShares >. val 0
            orderBy [ desc (pledge ^. PledgeFundedShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        last_paydays <- case projectLastPayday project of
            Nothing -> return []
            Just last_payday -> select $ from $ \ payday -> do
                where_ $ payday ^. PaydayId <=. val last_payday
                orderBy [ desc $ payday ^. PaydayId ]
                limit 2
                return payday

        user_payouts <- select $ from $ \ (transaction `InnerJoin` user) -> do
            where_ $ transaction ^. TransactionPayday `in_` valList (map (Just . entityKey) last_paydays)
            on_ $ transaction ^. TransactionDebit ==. just (user ^. UserAccount)
            groupBy $ user ^. UserId
            return (user ^. UserId, count $ transaction ^. TransactionId)

        return (project, pledges, M.fromList $ map ((\ (Value x :: Value UserId) -> x) *** (\ (Value x :: Value Int) -> x)) user_payouts)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Patrons | Snowdrift.coop"
        $(widgetFile "project_patrons")

--------------------------------------------------------------------------------
-- /t

getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    muser_id <- maybeAuthId
    (project, tagged_tickets) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        tagged_tickets <- fetchProjectTaggedTicketsDB project_id muser_id
        return (project, tagged_tickets)

    ((result, formWidget), encType) <- runFormGet viewForm
    let (filter_expression, order_expression) = case result of
            FormSuccess x -> x
            _ -> (defaultFilter, defaultOrder)

    github_issues <- getGithubIssues project

    let issues = sortBy (flip compare `on` order_expression . issueOrderable) $
                   filter (filter_expression . issueFilterable) $
                      map mkSomeIssue tagged_tickets ++ map mkSomeIssue github_issues

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Tickets | Snowdrift.coop"
        $(widgetFile "tickets")

--------------------------------------------------------------------------------
-- /transactions

getProjectTransactionsR :: Text -> Handler Html
getProjectTransactionsR project_handle = do
    (project, account, account_map, transaction_groups) <- runYDB $ do
        Entity _ project :: Entity Project <- getBy404 $ UniqueProjectHandle project_handle

        account <- get404 $ projectAccount project

        transactions <- select $ from $ \ t -> do
            where_ $ t ^. TransactionCredit ==. val (Just $ projectAccount project)
                    ||. t ^. TransactionDebit ==. val (Just $ projectAccount project)

            orderBy [ desc $ t ^. TransactionTs ]
            return t

        let accounts = S.toList $ S.fromList $ concatMap (\ (Entity _ t) -> maybeToList (transactionCredit t) <> maybeToList (transactionDebit t)) transactions

        users_by_account <- fmap (M.fromList . map (userAccount . entityVal &&& Right)) $ select $ from $ \ u -> do
            where_ $ u ^. UserAccount `in_` valList accounts
            return u

        projects_by_account <- fmap (M.fromList . map (projectAccount . entityVal &&& Left)) $ select $ from $ \ p -> do
            where_ $ p ^. ProjectAccount `in_` valList accounts
            return p

        let account_map = projects_by_account `M.union` users_by_account

        payday_map <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ pd -> do
            where_ $ pd ^. PaydayId `in_` valList (S.toList $ S.fromList $ mapMaybe (transactionPayday . entityVal) transactions)
            return pd

        return (project, account, account_map, process payday_map transactions)

    let getOtherAccount transaction
            | transactionCredit transaction == Just (projectAccount project) = transactionDebit transaction
            | transactionDebit transaction == Just (projectAccount project) = transactionCredit transaction
            | otherwise = Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Transactions | Snowdrift.coop"
        $(widgetFile "project_transactions")

  where
    process payday_map =
        let process' [] [] = []
            process' (t':ts') [] = [(fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts'))]
            process' [] (t:ts) = process' [t] ts

            process' (t':ts') (t:ts)
                | transactionPayday (entityVal t') == transactionPayday (entityVal t)
                = process' (t:t':ts') ts
                | otherwise
                = (fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts')) : process' [t] ts
         in process' []

--------------------------------------------------------------------------------
-- /w

getWikiPagesR :: Text -> Handler Html
getWikiPagesR project_handle = do
    muser_id <- maybeAuthId
    -- TODO: should be be using unviewed_comments and unviewed_edits?
    (project, pages, _, _) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pages <- getProjectWikiPages project_id

        -- If the user is not logged in or not watching the project, these maps are empty.
        (unviewed_comments, unviewed_edits) <- case muser_id of
            Nothing -> return (mempty, mempty)
            Just user_id -> do
                is_watching <- userIsWatchingProjectDB user_id project_id
                if is_watching
                    then (,)
                        <$> fetchNumUnviewedCommentsOnProjectWikiPagesDB user_id project_id
                        <*> fetchNumUnviewedWikiEditsOnProjectDB         user_id project_id
                    else return (mempty, mempty)
        return (project, pages, unviewed_comments, unviewed_edits)
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"
        $(widgetFile "wiki_pages")

--------------------------------------------------------------------------------
-- /watch, /unwatch

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "watching "
postUnwatchProjectR = watchOrUnwatchProject userUnwatchProjectDB "no longer watching "

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject action msg project_id = do
    user_id <- requireAuthId
    project <- runYDB $ do
        action user_id project_id
        get404 project_id
    alertSuccess (msg <> projectName project)
    redirect HomeR
