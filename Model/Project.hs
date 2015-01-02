module Model.Project
    ( ProjectSummary(..)
    , UpdateProject(..)
    , fetchPublicProjectsDB
    , fetchProjectDB
    , fetchProjectCommentRethreadEventsBeforeDB
    , fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB
    , fetchProjectCommentClosingEventsBeforeDB
    , fetchProjectDeletedPledgeEventsBeforeDB
    , fetchProjectDiscussionsDB
    , fetchProjectNewPledgeEventsBeforeDB
    , fetchProjectModeratorsDB
    , fetchProjectTeamMembersDB
    , fetchProjectOpenTicketsDB
    , fetchProjectUpdatedPledgeEventsBeforeDB
    , fetchProjectVolunteerApplicationsDB
    , fetchProjectWikiEditEventsWithTargetsBeforeDB
    , fetchProjectWikiPageEventsWithTargetsBeforeDB
    , fetchProjectBlogPostEventsBeforeDB
    , fetchProjectTicketClaimingEventsBeforeDB
    , fetchProjectTicketUnclaimingEventsBeforeDB
    , fetchProjectWikiPageByNameDB
    , insertProjectPledgeDB
    -- TODO(mitchell): rename all these... prefix fetch, suffix DB
    , getGithubIssues
    , getProjectPages
    , getProjectShares
    , getProjectTagList
    , getProjectWikiPages
    , projectComputeShareValue
    , projectNameWidget
    , summarizeProject
    , updateShareValue
    ) where

import Import

import Data.Filter
import Data.Order
import Model.Comment
import Model.Comment.Sql
import Model.Currency
import Model.Issue
import Model.Tag
import Model.Wiki.Sql
import Widgets.Tag

import           Control.Monad.Trans.Maybe    (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Writer.Strict  (tell)
import           Control.Concurrent.Async     (Async, async, wait)
import qualified Github.Data                  as GH
import qualified Github.Issues                as GH
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T

import           Data.List                    (sortBy)

--------------------------------------------------------------------------------
-- Types

data ProjectSummary = ProjectSummary
    { summaryName          :: Text
    , summaryProjectHandle :: Text
    , summaryUsers         :: UserCount
    , summaryShares        :: ShareCount
    , summaryShareCost     :: Milray
    }

data UpdateProject = UpdateProject
    { updateProjectName        :: Text
    , updateProjectDescription :: Markdown
    , updateProjectTags        :: [Text]
    , updateProjectGithubRepo  :: Maybe Text
    } deriving Show

newtype TaggedTicket = TaggedTicket ((Entity Ticket), [AnnotatedTag])

instance Issue TaggedTicket where
    issueWidget (TaggedTicket ((Entity ticket_id ticket),tags)) =
        [whamlet|
          <tr>
            <td>
              <a href="@{CommentDirectLinkR (ticketComment ticket)}">
                SD-#{toPathPiece ticket_id}
            <td>
              #{ticketName ticket}
            <td>
              $forall tag <- tags
                ^{tagWidget tag}
        |]
    issueFilterable = ticketToFilterable
    issueOrderable = ticketToOrderable

ticketToFilterable :: TaggedTicket -> Filterable
ticketToFilterable (TaggedTicket (Entity _ ticket, tags)) = Filterable has_tag get_named_ts search_literal
  where
    has_tag t = any (\ tag -> annotTagName tag == t && annotTagScore tag > 0) tags

    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)

ticketToOrderable :: TaggedTicket -> Orderable
ticketToOrderable (TaggedTicket ((Entity _ ticket),tags)) = Orderable has_tag get_named_ts search_literal
  where
    has_tag t = elem t $ map annotTagName tags
    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)

--------------------------------------------------------------------------------
-- Database actions

fetchPublicProjectsDB :: DB [Entity Project]
fetchPublicProjectsDB = select $ from $ \ p -> do
    where_ $ p ^. ProjectPublic
    return p

fetchProjectDB :: ProjectId -> DB [Entity Project]
fetchProjectDB project_id =
    select $ from $ \ p -> do
        where_ $ p ^. ProjectId ==. val project_id
        return p

insertProjectPledgeDB :: UserId
                      -> ProjectId
                      -> Int64
                      -> PledgeFormRenderedId
                      -> SDB ()
insertProjectPledgeDB user_id project_id shares pledge_render_id = do
    now <- liftIO getCurrentTime
    let shares_pledged = SharesPledged now user_id project_id shares pledge_render_id
    shares_pledged_id <- lift (insert shares_pledged)
    getBy (UniquePledge user_id project_id) >>= \case
        Nothing -> do
            lift $ insert_ (Pledge now user_id project_id shares shares)
            tell [ENewPledge shares_pledged_id shares_pledged]
        Just (Entity pledge_id old_pledge) -> do
            if shares == 0
                then do
                    lift (deleteKey pledge_id)
                    tell [EDeletedPledge now user_id project_id (pledgeShares old_pledge)]
                else do
                    lift $
                        update $ \ p -> do
                        set p [ PledgeShares       =. val shares
                              , PledgeFundedShares =. val shares
                              ]
                        where_ (p ^. PledgeId ==. val pledge_id)
                    tell [EUpdatedPledge (pledgeShares old_pledge) shares_pledged_id shares_pledged]

getGithubIssues :: Project -> Handler [GH.Issue]
getGithubIssues project =
    getGithubIssues'
    >>= liftIO . wait
    >>= either (\ _ -> addAlert "danger" "failed to fetch GitHub tickets\n" >> return [])
               return
  where
    getGithubIssues' :: Handler (Async (Either GH.Error [GH.Issue]))
    getGithubIssues' = liftIO . async $
        maybe
            (return $ Right [])
            (\ (account, repo) -> GH.issuesForRepo account repo [])
            parsedProjectGithubRepo

    parsedProjectGithubRepo :: Maybe (String, String)
    parsedProjectGithubRepo = second (drop 1) . break (== '/') . T.unpack <$> projectGithubRepo project

summarizeProject :: Monad m => Entity Project -> [Entity Pledge] -> m ProjectSummary
summarizeProject project pledges = do
    let share_value = projectShareValue $ entityVal project
        share_count = ShareCount $ sum . map (pledgeFundedShares . entityVal) $ pledges
        user_count = UserCount $ fromIntegral $ length pledges

    return $ ProjectSummary (projectName $ entityVal project) (projectHandle $ entityVal project) user_count share_count share_value


getProjectShares :: (MonadThrow m, MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadResource m) => ProjectId -> SqlPersistT m [Int64]
getProjectShares project_id = do
    pledges <- select $ from $ \ pledge -> do
        where_ ( pledge ^. PledgeProject ==. val project_id &&. pledge ^. PledgeFundedShares >. val 0)
        return pledge

    return $ map (pledgeFundedShares . entityVal) pledges

-- | Get all WikiPages for a Project.
getProjectPages :: ProjectId -> DB [Entity WikiPage]
getProjectPages project_id =
    select $
    from $ \ page -> do
    where_ $ page ^. WikiPageProject ==. val project_id
    return page

projectComputeShareValue :: [Int64] -> Milray
projectComputeShareValue pledges =
    let lg x = logBase 2 x :: Double
        num_users = fromIntegral $ length pledges
        geomean :: [Double] -> Double
        geomean xs = exp $ sum (map log xs) / fromIntegral (length xs)
        multiplier = lg (geomean (map fromIntegral pledges) * 2)
     in Milray 10 $* (multiplier * (num_users - 1))


-- signature needs to remain generic, for SnowdriftProcessPayments
updateShareValue :: (MonadThrow m, MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadResource m) => ProjectId -> SqlPersistT m ()
updateShareValue project_id = do
    pledges <- getProjectShares project_id

    update $ \ project -> do
        set project  [ ProjectShareValue =. val (projectComputeShareValue pledges) ]
        where_ (project ^. ProjectId ==. val project_id)

{-
 - TODO
 -  Unfund shares
 -  Fix algorithm
 -}

projectNameWidget :: ProjectId -> Widget
projectNameWidget project_id = do
    maybe_project <- handlerToWidget $ runDB $ get project_id
    case maybe_project of
        Nothing -> [whamlet| (unknown project) |]
        Just project -> [whamlet| #{projectName project} |]

getProjectTagList :: ProjectId -> DB ([Entity Tag], [Entity Tag])
getProjectTagList project_id = (,) <$> getProjectTags <*> getOtherTags
  where
    getProjectTags :: DB [Entity Tag]
    getProjectTags =
        selectDistinct $
        from $ \ (tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject ==. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

    getOtherTags :: DB [Entity Tag]
    getOtherTags =
        selectDistinct $
        from $ \ (tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject !=. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

-- | Get all of a Project's WikiPages, sorted alphabetically.
getProjectWikiPages :: [Language] -> ProjectId ->  DB [Entity WikiTarget]
getProjectWikiPages languages project_id = do
    targets <- select $ from $ \ wt -> do
        where_ $ wt ^. WikiTargetProject ==. val project_id
        return wt

    return $ sortBy (compare `on` (wikiTargetTarget . entityVal)) $ pickTargetsByLanguage languages targets

-- | Fetch all Discussions that are somewhere on the given Project.
fetchProjectDiscussionsDB :: ProjectId -> DB [DiscussionId]
fetchProjectDiscussionsDB project_id = do
    pd <- projectDiscussion <$> getJust project_id

    wpds <- fmap unwrapValues $ select $ from $ \ wp -> do
        where_ $ wp ^. WikiPageProject ==. val project_id
        return $ wp ^. WikiPageDiscussion

    bpds <- fmap unwrapValues $ select $ from $ \ bp -> do
        where_ $ bp ^. BlogPostProject ==. val project_id
        return $ bp ^. BlogPostDiscussion

    return (pd : wpds ++ bpds)

-- | Get all (posted, not pending) Comments made *somewhere* on a Project, before the given time.
fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [(EventCommentPostedId, Entity Comment)]
fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB project_id muser_id before lim = fetchProjectDiscussionsDB project_id >>= \ project_discussions ->
    fmap unwrapValues $ select $ from $ \ (ecp `InnerJoin` c) -> do
        on_ $ ecp ^. EventCommentPostedComment ==. c ^. CommentId
        where_ $ ecp ^. EventCommentPostedTs <=. val before
            &&. exprCommentProjectPermissionFilterIncludingRethreaded muser_id (val project_id) c
            &&. c ^. CommentDiscussion `in_` valList project_discussions
        orderBy [ desc $ ecp ^. EventCommentPostedTs, desc $ ecp ^. EventCommentPostedId ]
        limit lim
        return (ecp ^. EventCommentPostedId, c)

-- | Get all Rethreads whose *destinations* are on the given Project.
fetchProjectCommentRethreadEventsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [(EventCommentRethreadedId, Entity Rethread)]
fetchProjectCommentRethreadEventsBeforeDB project_id muser_id before lim = fetchProjectDiscussionsDB project_id >>= \ project_discussions ->
    fmap unwrapValues $ select $ from $ \ (ecr `InnerJoin` r `InnerJoin` c) -> do
        on_ $ r ^. RethreadNewComment ==. c ^. CommentId
        on_ $ ecr ^. EventCommentRethreadedRethread ==. r ^. RethreadId
        where_ $ ecr ^. EventCommentRethreadedTs <=. val before
            &&. exprCommentProjectPermissionFilter muser_id (val project_id) c
            &&. c ^. CommentDiscussion `in_` valList project_discussions
        orderBy [ desc $ ecr ^. EventCommentRethreadedTs, desc $ ecr ^. EventCommentRethreadedId ]
        limit lim
        return (ecr ^. EventCommentRethreadedId, r)

-- | Get all Closings for comments on the current project
fetchProjectCommentClosingEventsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [(EventCommentClosingId, Entity CommentClosing)]
fetchProjectCommentClosingEventsBeforeDB project_id muser_id before lim = fetchProjectDiscussionsDB project_id >>= \ project_discussions ->
    fmap unwrapValues $ select $ from $ \ (ecc `InnerJoin` cc `InnerJoin` c) -> do
        on_ $ cc ^. CommentClosingComment ==. c ^. CommentId
        on_ $ ecc ^. EventCommentClosingCommentClosing ==. cc ^. CommentClosingId
        where_ $ ecc ^. EventCommentClosingTs <=. val before
            &&. exprCommentProjectPermissionFilter muser_id (val project_id) c
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy [ desc $ cc ^. CommentClosingTs, desc $ cc ^. CommentClosingId ]
        limit lim
        return (ecc ^. EventCommentClosingId, cc)

fetchProjectTicketClaimingEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventTicketClaimedId, Either (Entity TicketClaiming) (Entity TicketOldClaiming))]
fetchProjectTicketClaimingEventsBeforeDB project_id before lim = do
    project_discussions <- fetchProjectDiscussionsDB project_id

    tuples <- fmap unwrapValues $ select $ from $ \ (etc `LeftOuterJoin` tc `LeftOuterJoin` toc `LeftOuterJoin` c `InnerJoin` t) -> do
        on_ $ t ^. TicketComment                ==. c   ^. CommentId
        on_ $ just (c ^. CommentId)             ==. tc  ?. TicketClaimingTicket
          ||. just (c ^. CommentId)             ==. toc ?. TicketOldClaimingTicket
        on_ $ etc ^. EventTicketClaimedOldClaim ==. toc ?. TicketOldClaimingId
        on_ $ etc ^. EventTicketClaimedClaim    ==. tc  ?. TicketClaimingId

        where_ $ etc ^. EventTicketClaimedTs <=. val before
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy [ desc $ etc ^. EventTicketClaimedTs, desc $ etc ^. EventTicketClaimedId ]
        limit lim
        return (etc ^. EventTicketClaimedId, tc, toc)

    let tupleToMaybeEither (etc_id, Just x, Nothing) = Just $ (etc_id, Left x)
        tupleToMaybeEither (etc_id, Nothing, Just y) = Just $ (etc_id, Right y)
        tupleToMaybeEither _ = Nothing

    return $ mapMaybe tupleToMaybeEither tuples

fetchProjectTicketUnclaimingEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventTicketUnclaimedId, Entity TicketOldClaiming)]
fetchProjectTicketUnclaimingEventsBeforeDB project_id before lim = fetchProjectDiscussionsDB project_id >>= \ project_discussions ->
    fmap unwrapValues $ select $ from $ \ (etu `InnerJoin` toc `InnerJoin` c `InnerJoin` t) -> do
        on_ $ t ^. TicketComment ==. c ^. CommentId
        on_ $ toc ^. TicketOldClaimingTicket ==. c ^. CommentId
        on_ $ etu ^. EventTicketUnclaimedClaim ==. toc ^. TicketOldClaimingId

        where_ $ etu ^. EventTicketUnclaimedTs <=. val before
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy [ desc $ etu ^. EventTicketUnclaimedTs, desc $ etu ^. EventTicketUnclaimedId ]
        limit lim
        return (etu ^. EventTicketUnclaimedId, toc)

-- | Fetch all WikiPages made on this Project before this time.
fetchProjectWikiPageEventsWithTargetsBeforeDB :: [Language] -> ProjectId -> UTCTime -> Int64 -> DB [(EventWikiPageId, Entity WikiPage, WikiTarget)]
fetchProjectWikiPageEventsWithTargetsBeforeDB languages project_id before lim = do
    pages <- fmap unwrapValues $ select $ from $ \ (ewp `InnerJoin` wp) -> do
        on_ (ewp ^. EventWikiPageWikiPage ==. wp ^. WikiPageId)
        where_ $ ewp ^. EventWikiPageTs <=. val before
            &&. exprWikiPageOnProject wp project_id
        orderBy [ desc $ ewp ^. EventWikiPageTs, desc $ ewp ^. EventWikiPageId ]
        limit lim
        return (ewp ^. EventWikiPageId, wp)

    targets <- select $ from $ \ wt -> do
        where_ $ wt ^. WikiTargetPage `in_` valList (map (entityKey . snd) pages)
        return wt


    let events = map (second entityKey) pages
        pages_map = M.fromList $ map ((entityKey &&& id) . snd) pages
        targets_map = M.fromList $ map (\ (Entity _ wiki_target) -> (wikiTargetPage wiki_target, wiki_target)) $ pickTargetsByLanguage languages targets

    return $ mapMaybe (\ (ewp_id, page_id) -> (ewp_id,,) <$> M.lookup page_id pages_map <*> M.lookup page_id targets_map) events

-- | Fetch all BlogPosts made on this Project before this time.
fetchProjectBlogPostEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventBlogPostId, Entity BlogPost)]
fetchProjectBlogPostEventsBeforeDB project_id before lim =
    fmap unwrapValues $ select $ from $ \ (ebp `InnerJoin` bp) -> do
        on_ $ ebp ^. EventBlogPostPost ==. bp ^. BlogPostId
        where_ $ ebp ^. EventBlogPostTs <=. val before
            &&. bp ^. BlogPostProject ==. val project_id

        orderBy [ desc $ ebp ^. EventBlogPostTs, desc $ ebp ^. EventBlogPostId ]
        limit lim
        return (ebp ^. EventBlogPostId, bp)

-- | Fetch all WikiEdits made on this Project before this time.
fetchProjectWikiEditEventsWithTargetsBeforeDB :: [Language] -> ProjectId -> UTCTime -> Int64 -> DB [(EventWikiEditId, Entity WikiEdit, WikiTarget)]
fetchProjectWikiEditEventsWithTargetsBeforeDB languages project_id before lim = do
-- | Fetch all WikiPages made on this Project before this time.

    edits <- fmap unwrapValues $ select $ from $ \ (ewe `InnerJoin` we `InnerJoin` wp) -> do
        on_ $ wp ^. WikiPageId ==. we ^. WikiEditPage
        on_ $ ewe ^. EventWikiEditWikiEdit ==. we ^. WikiEditId

        where_ $ ewe ^. EventWikiEditTs <=. val before
            &&.  exprWikiPageOnProject wp project_id

        orderBy [ desc $ ewe ^. EventWikiEditTs, desc $ ewe ^. EventWikiEditId ]
        limit lim
        return (ewe ^. EventWikiEditId, we)

    targets <- select $ from $ \ wt -> do
        where_ $ wt ^. WikiTargetPage `in_` valList (map (wikiEditPage . entityVal . snd) edits)
        return wt

    let events = map (id *** (entityKey &&& wikiEditPage . entityVal)) edits
        edits_map = M.fromList $ map ((entityKey &&& id) . snd) edits
        targets_map = M.fromList $ map ((wikiTargetPage &&& id) . entityVal) $ pickTargetsByLanguage languages targets

    return $ mapMaybe (\ (ewe_id, (edit_id, page_id)) -> (ewe_id,,) <$> M.lookup edit_id edits_map <*> M.lookup page_id targets_map) events

-- | Fetch all new SharesPledged made on this Project before this time.
fetchProjectNewPledgeEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventNewPledgeId, Entity SharesPledged)]
fetchProjectNewPledgeEventsBeforeDB project_id before lim =
    fmap unwrapValues $ select $ from $ \ (enp `InnerJoin` sp) -> do
        on_ $ enp ^. EventNewPledgeSharesPledged ==. sp ^. SharesPledgedId
        where_ $ enp ^. EventNewPledgeTs <=. val before
            &&. sp ^. SharesPledgedProject ==. val project_id
        orderBy [ desc $ enp ^. EventNewPledgeTs, desc $ enp ^. EventNewPledgeId ]
        limit lim
        return (enp ^. EventNewPledgeId, sp)

-- | Fetch all updated Pledges made on this Project before this time, along with the old number of shares.
fetchProjectUpdatedPledgeEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventUpdatedPledgeId, Int64, Entity SharesPledged)]
fetchProjectUpdatedPledgeEventsBeforeDB project_id before lim =
    fmap unwrapValues $ select $ from $ \ (eup `InnerJoin` sp) -> do
        on_ $ eup ^. EventUpdatedPledgeSharesPledged ==. sp ^. SharesPledgedId
        where_ $ eup ^. EventUpdatedPledgeTs <=. val before
            &&. sp ^. SharesPledgedProject ==. val project_id
        orderBy [ desc $ eup ^. EventUpdatedPledgeTs, desc $ eup ^. EventUpdatedPledgeId ]
        limit lim
        return (eup ^. EventUpdatedPledgeId, eup ^. EventUpdatedPledgeOldShares, sp)

-- | Fetch all deleted pledge events made on this Project before this time.
fetchProjectDeletedPledgeEventsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(EventDeletedPledgeId, EventDeletedPledge)]
fetchProjectDeletedPledgeEventsBeforeDB project_id before lim =
    fmap (map $ onEntity (,)) $ select $ from $ \ edp -> do
        where_ $ edp ^. EventDeletedPledgeTs <=. val before
            &&. edp ^. EventDeletedPledgeProject ==. val project_id

        orderBy [ desc $ edp ^. EventDeletedPledgeTs, desc $ edp ^. EventDeletedPledgeId ]
        limit lim
        return edp

-- | Fetch this Project's team members.
fetchProjectTeamMembersDB :: ProjectId -> DB [UserId]
fetchProjectTeamMembersDB = fetchProjectRoleDB TeamMember

fetchProjectModeratorsDB :: ProjectId -> DB [UserId]
fetchProjectModeratorsDB = fetchProjectRoleDB Moderator

-- | Abstract fetching Project Admins, TeamMembers, etc. Not exported.
fetchProjectRoleDB :: Role -> ProjectId -> DB [UserId]
fetchProjectRoleDB role project_id = fmap (map unValue) $
    select $
    from $ \ pur -> do
    where_ $
        pur ^. ProjectUserRoleProject ==. val project_id &&.
        pur ^. ProjectUserRoleRole    ==. val role
    return (pur ^. ProjectUserRoleUser)
  --
-- | Fetch all Project VolunteerApplications.
fetchProjectVolunteerApplicationsDB :: ProjectId -> DB [Entity VolunteerApplication]
fetchProjectVolunteerApplicationsDB project_id =
    select $
    from $ \ va -> do
    where_ (va ^. VolunteerApplicationProject ==. val project_id)
    orderBy [desc (va ^. VolunteerApplicationCreatedTs)]
    return va

-- | Fetch a WikiPage (maybe), given the Project handle and WikiPage target.
-- (Presumably, these Texts come from something like a rethread form,
-- where the user types in URLs manually).
fetchProjectWikiPageByNameDB :: Text -> Language -> Text -> DB (Maybe (Entity WikiPage))
fetchProjectWikiPageByNameDB project_handle language target = runMaybeT $ do
    Entity project_id _ <- MaybeT $ getBy $ UniqueProjectHandle project_handle
    Entity _ wiki_target <- MaybeT $ getBy $ UniqueWikiTarget project_id language target
    wiki_page <- MaybeT $ get $ wikiTargetPage wiki_target
    return $ Entity (wikiTargetPage wiki_target) wiki_page

fetchProjectOpenTicketsDB :: ProjectId -> Maybe UserId -> DB [TaggedTicket]
fetchProjectOpenTicketsDB project_id muser_id = do
    tickets <- fetchProjectDiscussionsDB project_id >>= fetch_tickets
    annot_tags_map <- fetchCommentCommentTagsInDB (map (ticketComment . entityVal) tickets) >>= buildAnnotatedCommentTagsDB muser_id
    let tagTicket :: Entity Ticket -> TaggedTicket
        tagTicket t@(Entity _ ticket) = TaggedTicket (t, M.findWithDefault [] (ticketComment ticket) annot_tags_map)
    return (map tagTicket tickets)
  where
    fetch_tickets discussion_ids =
        select $
        from $ \ (t `InnerJoin` c) -> do
        on_ (t ^. TicketComment ==. c ^. CommentId)
        where_ $
            c ^. CommentDiscussion `in_` valList discussion_ids &&.
            exprCommentOpen c
        return t
