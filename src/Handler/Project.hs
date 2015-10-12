{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import Data.Default (def)
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Data.Tree (Forest, Tree)
import System.Random (randomIO)
import Text.Cassius (cassiusFile)
import Text.Printf
import Yesod.AtomFeed
import Yesod.RssFeed
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Tree as Tree

import Data.Filter
import Data.Order
import Data.Time.Format
import Handler.Comment as Com
import Handler.Discussion
import Handler.Utils
import Model.Application
import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.HandlerInfo
import Model.Comment.Mods
import Model.Comment.Sql
import Model.Count
import Model.Currency
import Model.Discussion
import Model.Issue
import Model.Markdown.Diff
import Model.Project
import Model.Role
import Model.Shares
import Model.SnowdriftEvent
import Model.User
import Model.Utils
import Model.Wiki
import View.Comment
import View.Project
import View.SnowdriftEvent
import View.Time
import Widgets.Preview

--------------------------------------------------------------------------------
-- Utility functions

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def_val = do
    maybe_value <- lookupGetParam name
    return (fromMaybe def_val (maybe_value >>= readMaybe . T.unpack))

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

-- | Sanity check for Project Comment pages. Redirects if the comment was rethreaded.
-- 404's if the comment doesn't exist. 403 if permission denied.
checkComment :: Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Comment)
checkComment project_handle comment_id = do
    muser <- maybeAuth
    (project, comment) <- checkComment' (entityKey <$> muser) project_handle comment_id
    return (muser, project, comment)

-- | Like checkComment, but authentication is required.
checkCommentRequireAuth :: Text -> CommentId -> Handler (Entity User, Entity Project, Comment)
checkCommentRequireAuth project_handle comment_id = do
    user@(Entity user_id _) <- requireAuth
    (project, comment) <- checkComment' (Just user_id) project_handle comment_id
    return (user, project, comment)

-- | Abstract checkComment and checkCommentRequireAuth. You shouldn't use this function directly.
checkComment' :: Maybe UserId -> Text -> CommentId -> Handler (Entity Project, Comment)
checkComment' muser_id project_handle comment_id = do
    redirectIfRethreaded comment_id

    (project, ecomment) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        ecomment <- fetchCommentDB comment_id has_permission
        return (project, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                ->
            if commentDiscussion comment /= projectDiscussion (entityVal project)
                then notFound
                else return (project, comment)

checkProjectCommentActionPermission
        :: (CommentActionPermissions -> Bool)
        -> Entity User
        -> Text
        -> Entity Comment
        -> Handler ()
checkProjectCommentActionPermission
        can_perform_action
        user
        project_handle
        comment@(Entity comment_id _) = do
    action_permissions <-
        lookupErr "checkProjectCommentActionPermission: comment id not found in map" comment_id
          <$> makeProjectCommentActionPermissionsMap (Just user) project_handle def [comment]
    unless (can_perform_action action_permissions)
           (permissionDenied "You don't have permission to perform this action.")

makeProjectCommentForestWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Forest (Entity Comment))
makeProjectCommentForestWidget
        muser
        project_id
        project_handle
        comments =

    makeCommentForestWidget
      (projectCommentHandlerInfo muser project_id project_handle)
      comments
      muser

makeProjectCommentTreeWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Entity Comment
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Tree (Entity Comment))
makeProjectCommentTreeWidget a b c d e f g h = do
    (widget, [tree]) <- makeProjectCommentForestWidget a b c [d] e f g h
    return (widget, tree)

makeProjectCommentActionWidget
        :: MakeCommentActionWidget
        -> Text
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeProjectCommentActionWidget make_comment_action_widget project_handle comment_id mods get_max_depth = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    let handler_info = projectCommentHandlerInfo (Just user) project_id project_handle
    make_comment_action_widget (Entity comment_id comment) user handler_info mods get_max_depth False

projectDiscussionPage :: Text -> Widget -> Widget
projectDiscussionPage project_handle widget = do
    $(widgetFile "project_discussion_wrapper")
    toWidget $(cassiusFile "templates/comment.cassius")


-------------------------------------------------------------------------------
--

getProjectsR :: Handler Html
getProjectsR = do
    project_summaries <- runDB $ do
        projects <- fetchPublicProjectsDB
        forM projects $ \project -> do
            pledges <- fetchProjectPledgesDB $ entityKey project
            discussions <- fetchProjectDiscussionsDB $ entityKey project
            tickets <- fetchProjectOpenTicketsDB (entityKey project) Nothing
            let summary = summarizeProject project pledges discussions tickets
            return (project, summary)

    let discussionsCount = getCount . summaryDiscussionCount
    let ticketsCount = getCount . summaryTicketCount

    defaultLayout $ do
        snowdriftTitle "Projects"
        $(widgetFile "projects")

--------------------------------------------------------------------------------
-- /

getProjectR :: Text -> Handler Html
getProjectR project_handle = do
    mviewer_id <- maybeAuthId

    (project_id, project, is_watching, pledges, pledge) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- fetchProjectSharesDB project_id
        (pledge, is_watching) <- case mviewer_id of
            Nothing -> return (Nothing, False)
            Just viewer_id -> (,)
                <$> getBy (UniquePledge viewer_id project_id)
                <*> userIsWatchingProjectDB viewer_id project_id
        return (project_id, project, is_watching, pledges, pledge)

    defaultLayout $ do
        snowdriftTitle $ projectName project
        renderProject (Just project_id) project mviewer_id is_watching pledges pledge

postProjectR :: Text -> Handler Html
postProjectR project_handle = do
    (viewer_id, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject
                     name
                     blurb
                     description
                     tags
                     github_repo
                     logo) ->
            lookupPostMode >>= \case
                Just PostMode -> do
                    runDB $ do
                        when (projectBlurb project /= blurb) $ do
                            project_update <- insert $
                                ProjectUpdate now
                                              project_id
                                              viewer_id
                                              blurb
                                              (diffMarkdown
                                                  (projectDescription project)
                                                  description)
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity k _) -> repsert k $ ProjectLastUpdate project_id project_update
                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update

                        update $ \p -> do
                            set p [ ProjectName =. val name
                                  , ProjectBlurb =. val blurb
                                  , ProjectDescription =. val description
                                  , ProjectGithubRepo =. val github_repo
                                  , ProjectLogo =. val logo
                                  ]
                            where_ (p ^. ProjectId ==. val project_id)

                        tag_ids <- forM tags $ \tag_name -> do
                            tag_entity_list <- select $ from $ \tag -> do
                                where_ (tag ^. TagName ==. val tag_name)
                                return tag

                            case tag_entity_list of
                                [] -> insert $ Tag tag_name
                                Entity tag_id _ : _ -> return tag_id


                        delete $
                         from $ \pt ->
                         where_ (pt ^. ProjectTagProject ==. val project_id)

                        forM_ tag_ids $ \tag_id -> insert (ProjectTag project_id tag_id)

                    alertSuccess "project updated"
                    redirect $ ProjectR project_handle

                _ -> do
                    let
                        preview_project = project
                            { projectName = name
                            , projectBlurb = blurb
                            , projectDescription = description
                            , projectGithubRepo = github_repo
                            , projectLogo = logo
                            }

                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))
                    defaultLayout $ previewWidget form "update" $ renderProject (Just project_id) preview_project Nothing False [] Nothing

        x -> do
            alertDanger $ T.pack $ show x
            redirect $ ProjectR project_handle

--------------------------------------------------------------------------------
-- /applications (List of submitted applications)

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
        snowdriftTitle $ projectName project <> " Volunteer Applications"
        $(widgetFile "applications")

--------------------------------------------------------------------------------
-- /application (Form for new application)

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
        snowdriftDashTitle
            (projectName project <> " Volunteer Application")
            (userDisplayName user)
        $(widgetFile "application")

--------------------------------------------------------------------------------
-- /edit

getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    (_, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    tags <- runDB $
        select $
        from $ \(p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayout $ do
        snowdriftTitle $ projectName project
        $(widgetFile "edit_project")

--------------------------------------------------------------------------------
-- /feed

-- | This function is responsible for hitting every relevant event table. Nothing
-- statically guarantees that.
getProjectFeedR :: Text -> Handler TypedContent
getProjectFeedR project_handle = do
    let lim = 26 -- limit 'lim' from each table, then take 'lim - 1'

    languages <- getLanguages

    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    before <- lookupGetUTCTimeDefaultNow "before"

    (
        project_id, project,
        is_watching,
        comment_events, rethread_events, closing_events, claiming_events, unclaiming_events,
        wiki_page_events, wiki_edit_events, blog_post_events, new_pledge_events,
        updated_pledge_events, deleted_pledge_events,

        discussion_map, wiki_target_map, user_map, earlier_closures_map, earlier_retracts_map,
        closure_map, retract_map, ticket_map, claim_map, flag_map
     ) <- runYDB $ do

        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        is_watching <- maybe (pure False) (flip userIsWatchingProjectDB project_id) muser_id
        comment_events        <- fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB     project_id muser_id before lim
        rethread_events       <- fetchProjectCommentRethreadEventsBeforeDB                      project_id muser_id before lim
        closing_events        <- fetchProjectCommentClosingEventsBeforeDB                       project_id muser_id before lim
        claiming_events       <- fetchProjectTicketClaimingEventsBeforeDB                       project_id before lim
        unclaiming_events     <- fetchProjectTicketUnclaimingEventsBeforeDB                     project_id before lim
        wiki_page_events      <- fetchProjectWikiPageEventsWithTargetsBeforeDB languages        project_id before lim
        blog_post_events      <- fetchProjectBlogPostEventsBeforeDB                             project_id before lim
        wiki_edit_events      <- fetchProjectWikiEditEventsWithTargetsBeforeDB languages        project_id before lim
        new_pledge_events     <- fetchProjectNewPledgeEventsBeforeDB                            project_id before lim
        updated_pledge_events <- fetchProjectUpdatedPledgeEventsBeforeDB                        project_id before lim
        deleted_pledge_events <- fetchProjectDeletedPledgeEventsBeforeDB                        project_id before lim

        -- Suplementary maps for displaying the data. If something above requires extra
        -- data to display the project feed row, it MUST be used to fetch the data below!

        let (comment_ids, comment_users)        = F.foldMap (\(_, Entity comment_id comment) -> ([comment_id], [commentUser comment])) comment_events
            (wiki_edit_users, wiki_edit_pages)  = F.foldMap (\(_, Entity _ e, _) -> ([wikiEditUser e], [wikiEditPage e])) wiki_edit_events
            (blog_post_users)                   = F.foldMap (\(_, Entity _ e) -> [blogPostUser e]) blog_post_events
            shares_pledged                      = map (entityVal . snd) new_pledge_events <> map (\(_, _, x) -> entityVal x) updated_pledge_events
            closing_users                       = map (commentClosingClosedBy . entityVal . snd) closing_events
            rethreading_users                   = map (rethreadModerator . entityVal . snd) rethread_events
            ticket_claiming_users               = map (either (ticketClaimingUser . entityVal) (ticketOldClaimingUser . entityVal) . snd) claiming_events
            ticket_unclaiming_users             = map (ticketOldClaimingUser . entityVal . snd) unclaiming_events
            pledging_users                      = map sharesPledgedUser shares_pledged
            unpledging_users                    = map (eventDeletedPledgeUser . snd) deleted_pledge_events

            -- All users: comment posters, wiki page creators, etc.
            user_ids = S.toList $ mconcat
                        [ S.fromList comment_users
                        , S.fromList closing_users
                        , S.fromList rethreading_users
                        , S.fromList wiki_edit_users
                        , S.fromList blog_post_users
                        , S.fromList ticket_claiming_users
                        , S.fromList ticket_unclaiming_users
                        , S.fromList pledging_users
                        , S.fromList unpledging_users
                        ]

        discussion_map <- fetchProjectDiscussionsDB project_id >>= fetchDiscussionsDB languages

        let claimed_comment_ids = map (either (ticketClaimingTicket . entityVal) (ticketOldClaimingTicket . entityVal) . snd) claiming_events
            unclaimed_comment_ids = map (ticketOldClaimingTicket . entityVal . snd) unclaiming_events
            closed_comment_ids = map (commentClosingComment . entityVal . snd) closing_events

        ticket_map <- fetchCommentTicketsDB $ mconcat
            [ S.fromList comment_ids
            , S.fromList claimed_comment_ids
            , S.fromList unclaimed_comment_ids
            , S.fromList closed_comment_ids
            ]

        -- WikiPages keyed by their own IDs (contained in a WikiEdit)
        wiki_targets <- pickTargetsByLanguage languages <$> fetchWikiPageTargetsInDB wiki_edit_pages
        let wiki_target_map = M.fromList $ map ((wikiTargetPage &&& id) . entityVal) wiki_targets

        user_map <- entitiesMap <$> fetchUsersInDB user_ids

        earlier_closures_map <- fetchCommentsAncestorClosuresDB comment_ids
        earlier_retracts_map <- fetchCommentsAncestorRetractsDB comment_ids
        closure_map          <- makeCommentClosingMapDB         comment_ids
        retract_map          <- makeCommentRetractingMapDB      comment_ids
        claim_map            <- makeClaimedTicketMapDB          comment_ids
        flag_map             <- makeFlagMapDB                   comment_ids

        return
            (
                project_id, project,
                is_watching,
                comment_events, rethread_events, closing_events, claiming_events, unclaiming_events, wiki_page_events,
                wiki_edit_events, blog_post_events, new_pledge_events, updated_pledge_events, deleted_pledge_events,

                discussion_map, wiki_target_map, user_map, earlier_closures_map, earlier_retracts_map,
                closure_map, retract_map, ticket_map, claim_map, flag_map
            )

    action_permissions_map <- makeProjectCommentActionPermissionsMap muser project_handle def (map snd comment_events)


    let all_unsorted_events :: [(Route App, SnowdriftEvent)]
        all_unsorted_events = mconcat
            [ map (EventCommentPostedR      *** onEntity ECommentPosted)        comment_events
            , map (EventCommentRethreadedR  *** onEntity ECommentRethreaded)    rethread_events
            , map (EventCommentClosingR     *** onEntity ECommentClosed)        closing_events

            , map (EventTicketClaimedR      *** ETicketClaimed . (onEntity (,) +++ onEntity (,))) claiming_events

            , map (EventTicketUnclaimedR    *** onEntity ETicketUnclaimed)      unclaiming_events

            , map (\(eid, Entity wpid wp, wt)
                    -> (EventWikiPageR eid, EWikiPage wpid wp wt))              wiki_page_events

            , map (\(eid, Entity weid we, wt)
                    -> (EventWikiEditR eid, EWikiEdit weid we wt))              wiki_edit_events

            , map (EventBlogPostR           *** onEntity EBlogPost)             blog_post_events
            , map (EventNewPledgeR          *** onEntity ENewPledge)            new_pledge_events

            , map (\(eid, shares, pledge)
                    -> (EventUpdatedPledgeR eid, eup2se shares pledge))         updated_pledge_events

            , map (EventDeletedPledgeR      *** edp2se)                         deleted_pledge_events
            ]

        (events, more_events) = splitAt (lim-1) (sortBy (snowdriftEventNewestToOldest `on` snd) all_unsorted_events)

        -- For pagination: Nothing means no more pages, Just time means set the 'before'
        -- GET param to that time. Note that this means 'before' should be a <= relation,
        -- rather than a <.
        mnext_before :: Maybe Text
        mnext_before = case more_events of
          []             -> Nothing
          ((_, next_event):_) -> (Just . T.pack . show . snowdriftEventTime) next_event

    now        <- liftIO getCurrentTime
    Just route <- getCurrentRoute
    render     <- getUrlRender

    let feed = Feed "project feed" route HomeR "Snowdrift Community" "" "en" now $
            mapMaybe (uncurry $ snowdriftEventToFeedEntry
                        render
                        project_handle
                        user_map
                        discussion_map
                        wiki_target_map
                        ticket_map) events

    selectRep $ do
        provideRep $ atomFeed feed
        provideRep $ rssFeed feed
        provideRep $ defaultLayout $ do
            snowdriftDashTitle (projectName project) "Feed"
            $(widgetFile "project_feed")
            toWidget $(cassiusFile "templates/comment.cassius")

  where
    -- "event updated pledge to snowdrift event"
    eup2se :: Int64 -> Entity SharesPledged -> SnowdriftEvent
    eup2se old_shares (Entity shares_pledged_id shares_pledged) = EUpdatedPledge old_shares shares_pledged_id shares_pledged

    -- "event deleted pledge to snowdrift event"
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
        from $ \invite -> do
        where_ ( invite ^. InviteRedeemed ==. val False )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        return invite

    redeemed_invites <- runDB $
        select $
        from $ \invite -> do
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
            let Entity _ user = fromMaybe (error "getInviteR: user_id not found in users map")
                                          (M.lookup user_id users)
             in fromMaybe (userIdent user) $ userName user

        format_inviter user_id =
            userDisplayName $ fromMaybe (error "getInviteR(#2): user_id not found in users map")
                                        (M.lookup user_id users)

    defaultLayout $ do
        snowdriftDashTitle (projectName project) "Send Invite"
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
        pledges <- select $ from $ \(pledge `InnerJoin` user) -> do
            on_ $ pledge ^. PledgeUser ==. user ^. UserId
            where_ $ pledge ^. PledgeProject ==. val project_id
                &&. pledge ^. PledgeFundedShares >. val 0
            orderBy [ desc (pledge ^. PledgeFundedShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        last_paydays <- case projectLastPayday project of
            Nothing -> return []
            Just last_payday -> select $ from $ \payday -> do
                where_ $ payday ^. PaydayId <=. val last_payday
                orderBy [ desc $ payday ^. PaydayId ]
                limit 2
                return payday

        user_payouts <- select $ from $ \(transaction `InnerJoin` user) -> do
            where_ $ transaction ^. TransactionPayday `in_` valList (map (Just . entityKey) last_paydays)
            on_ $ transaction ^. TransactionDebit ==. just (user ^. UserAccount)
            groupBy $ user ^. UserId
            return (user ^. UserId, count $ transaction ^. TransactionId)

        return (project, pledges, M.fromList $ map ((\(Value x :: Value UserId) -> x) *** (\(Value x :: Value Int) -> x)) user_payouts)

    defaultLayout $ do
        snowdriftTitle $ projectName project <> " Patrons"
        $(widgetFile "project_patrons")

--------------------------------------------------------------------------------
-- /pledge

getUpdatePledgeR :: Text -> Handler Html
getUpdatePledgeR project_handle = do
    _ <- requireAuthId
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    ((result, _), _) <- runFormGet $ pledgeForm project_id
    let dangerRedirect msg = do
            alertDanger msg
            redirect $ ProjectR project_handle
    case result of
        FormSuccess (SharesPurchaseOrder new_user_shares) -> do
            user_id <- requireAuthId

            (confirm_form, _) <- generateFormPost $ projectConfirmPledgeForm (Just new_user_shares)

            (mpledge, other_shares, pledges) <- runDB $ do
                pledges <- fetchProjectSharesDB project_id
                mpledge <- getBy $ UniquePledge user_id project_id
                other_shares <- fmap unwrapValues $ select $ from $ \p -> do
                    where_ $ p ^. PledgeProject ==. val project_id
                        &&. p ^. PledgeUser !=. val user_id

                    return $ p ^. PledgeShares

                return (mpledge, other_shares, pledges)

            let new_user_mills = millMilray new_user_shares
            case mpledge of
                Just (Entity _ pledge) | pledgeShares pledge == new_user_shares -> do
                    alertWarning $ T.unwords
                        [ "Your pledge was already at"
                        , T.pack (show new_user_mills) <> "."
                        , "Thank you for your support!"
                        ]

                    redirect (ProjectR project_handle)

                _ -> do
                    let old_user_shares = maybe 0 (pledgeShares . entityVal) mpledge
                        old_user_mills  = millMilray old_user_shares

                        numPatrons = toInteger $ length pledges

                        new_project_shares = filter (>0) [new_user_shares] ++ other_shares

                        old_project_shares = filter (>0) [old_user_shares] ++ other_shares

                        new_share_value = projectComputeShareValue new_project_shares
                        old_share_value = projectComputeShareValue old_project_shares

                        new_user_amount = new_share_value $* fromIntegral new_user_shares
                        old_user_amount = old_share_value $* fromIntegral old_user_shares

                        new_project_amount = new_share_value $* fromIntegral (sum new_project_shares)
                        old_project_amount = old_share_value $* fromIntegral (sum old_project_shares)

                        user_decrease    = old_user_amount - new_user_amount
                        user_increase    = new_user_amount - old_user_amount
                        project_decrease = old_project_amount - new_project_amount
                        project_increase = new_project_amount - old_project_amount
                        matching_drop   = project_decrease - user_decrease
                        matched_extra    = project_increase - new_user_amount

                    defaultLayout $ do
                        snowdriftDashTitle
                            (projectName project)
                            "update pledge"
                        $(widgetFile "update_pledge")

        FormMissing -> dangerRedirect "Form missing."
        FormFailure errors ->
            dangerRedirect $ T.snoc (T.intercalate "; " errors) '.'

postUpdatePledgeR :: Text -> Handler Html
postUpdatePledgeR project_handle = do
    ((result, _), _) <- runFormPost $ projectConfirmPledgeForm Nothing
    isConfirmed <- maybe False (T.isPrefixOf "yes") <$> lookupPostParam "confirm"

    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            when isConfirmed $ updateUserPledge project_handle shares
            redirect (ProjectR project_handle)
        _ -> do
            alertDanger "error occurred in form submission"
            redirect (UpdatePledgeR project_handle)

--------------------------------------------------------------------------------
-- /t

getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    muser_id <- maybeAuthId
    (project, tagged_tickets) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        tagged_tickets <- fetchProjectOpenTicketsDB project_id muser_id
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
        snowdriftTitle $ projectName project <> " Tickets"
        $(widgetFile "tickets")


--------------------------------------------------------------------------------
-- /t/#TicketId

getTicketR :: Text -> TicketId -> Handler ()
getTicketR project_handle ticket_id = do
    Ticket{..} <- runYDB $ do
        void $ getBy404 $ UniqueProjectHandle project_handle
        get404 ticket_id

    -- TODO - check that the comment is associated with the correct project

    redirect $ CommentDirectLinkR ticketComment


--------------------------------------------------------------------------------
-- /transactions

getProjectTransactionsR :: Text -> Handler Html
getProjectTransactionsR project_handle = do
    (project, account, account_map, transaction_groups) <- runYDB $ do
        Entity _ project :: Entity Project <- getBy404 $ UniqueProjectHandle project_handle

        account <- get404 $ projectAccount project

        transactions <- select $ from $ \t -> do
            where_ $ t ^. TransactionCredit ==. val (Just $ projectAccount project)
                    ||. t ^. TransactionDebit ==. val (Just $ projectAccount project)

            orderBy [ desc $ t ^. TransactionTs ]
            return t

        let accounts = S.toList $ S.fromList $ concatMap (\(Entity _ t) -> maybeToList (transactionCredit t) <> maybeToList (transactionDebit t)) transactions

        users_by_account <- fmap (M.fromList . map (userAccount . entityVal &&& Right)) $ select $ from $ \u -> do
            where_ $ u ^. UserAccount `in_` valList accounts
            return u

        projects_by_account <- fmap (M.fromList . map (projectAccount . entityVal &&& Left)) $ select $ from $ \p -> do
            where_ $ p ^. ProjectAccount `in_` valList accounts
            return p

        let account_map = projects_by_account `M.union` users_by_account

        payday_map <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \pd -> do
            where_ $ pd ^. PaydayId `in_` valList (S.toList $ S.fromList $ mapMaybe (transactionPayday . entityVal) transactions)
            return pd

        return (project, account, account_map, process payday_map transactions)

    let getOtherAccount transaction
            | transactionCredit transaction == Just (projectAccount project) = transactionDebit transaction
            | transactionDebit transaction == Just (projectAccount project) = transactionCredit transaction
            | otherwise = Nothing

    defaultLayout $ do
        snowdriftTitle $ projectName project <> " Transactions"
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
    void maybeAuthId
    languages <- getLanguages

    (project, wiki_targets) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        wiki_targets <- getProjectWikiPages languages project_id
        return (project, wiki_targets)
    defaultLayout $ do
        snowdriftTitle $ projectName project <> " Wiki"
        $(widgetFile "wiki_pages")

--------------------------------------------------------------------------------
-- /watch, /unwatch

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "Watching "
postUnwatchProjectR = watchOrUnwatchProject userUnwatchProjectDB "No longer watching "

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject action msg project_id = do
    user_id <- requireAuthId
    project <- runYDB $ do
        action user_id project_id
        get404 project_id
    alertSuccess (msg <> projectName project <> ".")
    redirect $ ProjectR $ projectHandle project

--------------------------------------------------------------------------------
-- /c/#CommentId

getProjectCommentR :: Text -> CommentId -> Handler Html
getProjectCommentR project_handle comment_id = do
    (muser, Entity project_id _, comment) <- checkComment project_handle comment_id
    (widget, comment_tree) <-
        makeProjectCommentTreeWidget
          muser
          project_id
          project_handle
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          mempty

    case muser of
        Nothing -> return ()
        Just (Entity user_id _) ->
            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))

    defaultLayout (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/approve

getApproveProjectCommentR :: Text -> CommentId -> Handler Html
getApproveProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeApproveCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postApproveProjectCommentR :: Text -> CommentId -> Handler Html
postApproveProjectCommentR project_handle comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_approve user project_handle (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/claim

getClaimProjectCommentR :: Text -> CommentId -> Handler Html
getClaimProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeClaimCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postClaimProjectCommentR :: Text -> CommentId -> Handler Html
postClaimProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_claim user project_handle (Entity comment_id comment)

    postClaimComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "claim" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/close

getCloseProjectCommentR :: Text -> CommentId -> Handler Html
getCloseProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeCloseCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)


postCloseProjectCommentR :: Text -> CommentId -> Handler Html
postCloseProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_close user project_handle (Entity comment_id comment)

    postCloseComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "close" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/delete

getDeleteProjectCommentR :: Text -> CommentId -> Handler Html
getDeleteProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeDeleteCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postDeleteProjectCommentR :: Text -> CommentId -> Handler Html
postDeleteProjectCommentR project_handle comment_id = do
    (user, _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_delete user project_handle (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect (ProjectDiscussionR project_handle)
        else redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/edit

getEditProjectCommentR :: Text -> CommentId -> Handler Html
getEditProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeEditCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postEditProjectCommentR :: Text -> CommentId -> Handler Html
postEditProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_edit user project_handle (Entity comment_id comment)

    postEditComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)         -- Edit made.
        Just (widget, form) -> defaultLayout $ previewWidget form "post" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/flag

getFlagProjectCommentR :: Text -> CommentId -> Handler Html
getFlagProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeFlagCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postFlagProjectCommentR :: Text -> CommentId -> Handler Html
postFlagProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_flag user project_handle (Entity comment_id comment)

    postFlagComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectDiscussionR project_handle)
        Just (widget, form) -> defaultLayout $ previewWidget form "flag" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/reply

getReplyProjectCommentR :: Text -> CommentId -> Handler Html
getReplyProjectCommentR project_handle parent_id = do
    (widget, _) <- makeProjectCommentActionWidget makeReplyCommentWidget project_handle parent_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postReplyProjectCommentR :: Text -> CommentId -> Handler Html
postReplyProjectCommentR project_handle parent_id = do
    (user, Entity _ project, parent) <- checkCommentRequireAuth project_handle parent_id
    checkProjectCommentActionPermission can_reply user project_handle (Entity parent_id parent)

    postNewComment
      (Just parent_id)
      user
      (projectDiscussion project)
      (makeProjectCommentActionPermissionsMap (Just user) project_handle def)
      >>= \case
          ConfirmedPost (Left err) -> do
              alertDanger err
              redirect $ ReplyProjectCommentR project_handle parent_id
          ConfirmedPost (Right _) ->
              redirect $ ProjectCommentR project_handle parent_id
          Com.Preview (widget, form) ->
              defaultLayout $ previewWidget form "post" $
                  projectDiscussionPage project_handle widget

--------------------------------------------------------------------------------
-- /c/#CommentId/rethread

getRethreadProjectCommentR :: Text -> CommentId -> Handler Html
getRethreadProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeRethreadCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postRethreadProjectCommentR :: Text -> CommentId -> Handler Html
postRethreadProjectCommentR project_handle comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_rethread user project_handle (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /c/#CommentId/retract

getRetractProjectCommentR :: Text -> CommentId -> Handler Html
getRetractProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeRetractCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postRetractProjectCommentR :: Text -> CommentId -> Handler Html
postRetractProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_retract user project_handle (Entity comment_id comment)

    postRetractComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "retract" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/tags

getProjectCommentTagsR :: Text -> CommentId -> Handler Html
getProjectCommentTagsR _ = getCommentTags

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/#TagId

getProjectCommentTagR :: Text -> CommentId -> TagId -> Handler Html
getProjectCommentTagR _ = getCommentTagR

postProjectCommentTagR :: Text -> CommentId -> TagId -> Handler ()
postProjectCommentTagR _ = postCommentTagR

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/apply, /c/#CommentId/tag/create

postProjectCommentApplyTagR, postProjectCommentCreateTagR:: Text -> CommentId -> Handler Html
postProjectCommentApplyTagR  = applyOrCreate postCommentApplyTag
postProjectCommentCreateTagR = applyOrCreate postCommentCreateTag

applyOrCreate :: (CommentId -> Handler ()) -> Text -> CommentId -> Handler Html
applyOrCreate action project_handle comment_id = do
    action comment_id
    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/new

getProjectCommentAddTagR :: Text -> CommentId -> Handler Html
getProjectCommentAddTagR project_handle comment_id = do
    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_add_tag user project_handle (Entity comment_id comment)
    getProjectCommentAddTag comment_id project_id user_id

--------------------------------------------------------------------------------
-- /c/#CommentId/unclaim

getUnclaimProjectCommentR :: Text -> CommentId -> Handler Html
getUnclaimProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeUnclaimCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postUnclaimProjectCommentR :: Text -> CommentId -> Handler Html
postUnclaimProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_unclaim user project_handle (Entity comment_id comment)

    postUnclaimComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "unclaim" (projectDiscussionPage project_handle widget)

--------------------------------------------------------------------------------
-- /c/#CommentId/watch

getWatchProjectCommentR :: Text -> CommentId -> Handler Html
getWatchProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeWatchCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postWatchProjectCommentR ::Text -> CommentId -> Handler Html
postWatchProjectCommentR project_handle comment_id = do
    (viewer@(Entity viewer_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_watch viewer project_handle (Entity comment_id comment)

    postWatchComment viewer_id comment_id

    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/unwatch

getUnwatchProjectCommentR :: Text -> CommentId -> Handler Html
getUnwatchProjectCommentR project_handle comment_id = do
    (widget, _) <- makeProjectCommentActionWidget makeUnwatchCommentWidget project_handle comment_id def getMaxDepth

    defaultLayout (projectDiscussionPage project_handle widget)

postUnwatchProjectCommentR ::Text -> CommentId -> Handler Html
postUnwatchProjectCommentR project_handle comment_id = do
    (viewer@(Entity viewer_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_watch viewer project_handle (Entity comment_id comment)

    postUnwatchComment viewer_id comment_id

    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /contact

-- ProjectContactR stuff posts a private new topic to project discussion

getProjectContactR :: Text -> Handler Html
getProjectContactR project_handle = do
    (project_contact_form, _) <- generateFormPost projectContactForm
    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)
    defaultLayout $ do
        snowdriftTitle $ "Contact " <> projectName project
        $(widgetFile "project_contact")

postProjectContactR :: Text -> Handler Html
postProjectContactR project_handle = do
    maybe_user_id <- maybeAuthId

    ((result, _), _) <- runFormPost projectContactForm

    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)

    case result of
        FormSuccess (content, language) -> do
            _ <- runSDB (postApprovedCommentDB (fromMaybe anonymousUser maybe_user_id) Nothing (projectDiscussion project) content VisPrivate language)

            alertSuccess "Comment submitted. Thank you for your input!"

        _ -> alertDanger "Error occurred when submitting form."

    redirect $ ProjectContactR project_handle

--------------------------------------------------------------------------------
-- /d

getProjectDiscussionR :: Text -> Handler Html
getProjectDiscussionR = getDiscussion . getProjectDiscussion

getProjectDiscussion :: Text -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html
getProjectDiscussion project_handle get_root_comments = do
    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    (Entity project_id project, root_comments) <- runYDB $ do
        p@(Entity project_id project) <- getBy404 (UniqueProjectHandle project_handle)
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        root_comments <- get_root_comments (projectDiscussion project) has_permission
        return (p, root_comments)

    (comment_forest_no_css, _) <-
        makeProjectCommentForestWidget
          muser
          project_id
          project_handle
          root_comments
          def
          getMaxDepth
          False
          mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        snowdriftTitle $ projectName project <> " Discussion"
        $(widgetFile "project_discuss")

--------------------------------------------------------------------------------
-- /d/new

getNewProjectDiscussionR :: Text -> Handler Html
getNewProjectDiscussionR project_handle = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout (projectDiscussionPage project_handle widget)

postNewProjectDiscussionR :: Text -> Handler Html
postNewProjectDiscussionR project_handle = do
    user <- requireAuth
    Entity _ Project{..} <- runYDB (getBy404 (UniqueProjectHandle project_handle))

    postNewComment
      Nothing
      user
      projectDiscussion
      (makeProjectCommentActionPermissionsMap (Just user) project_handle def)
      >>= \case
          ConfirmedPost (Left err) -> do
              alertDanger err
              redirect $ NewProjectDiscussionR project_handle
          ConfirmedPost (Right comment_id) ->
              redirect $ ProjectCommentR project_handle comment_id
          Com.Preview (widget, form) ->
              defaultLayout $ previewWidget form "post" $
                  projectDiscussionPage project_handle widget
