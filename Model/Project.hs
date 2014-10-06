module Model.Project
    ( ProjectSummary(..)
    , UpdateProject(..)
    , fetchPublicProjectsDB
    , fetchProjectCommentRethreadsBeforeDB
    , fetchProjectCommentsIncludingRethreadedBeforeDB
    , fetchProjectDeletedPledgesBeforeDB
    , fetchProjectDiscussionsDB
    , fetchProjectNewPledgesBeforeDB
    , fetchProjectModeratorsDB
    , fetchProjectTeamMembersDB
    , fetchProjectOpenTicketsDB
    , fetchProjectUpdatedPledgesBeforeDB
    , fetchProjectVolunteerApplicationsDB
    , fetchProjectWikiEditsBeforeDB
    , fetchProjectWikiPagesBeforeDB
    , fetchProjectBlogPostsBeforeDB
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
    has_tag t = any (\tag -> annotTagName tag == t && annotTagScore tag > 0) tags

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
                        update $ \p -> do
                        set p [ PledgeShares       =. val shares
                              , PledgeFundedShares =. val shares
                              ]
                        where_ (p ^. PledgeId ==. val pledge_id)
                    tell [EUpdatedPledge (pledgeShares old_pledge) shares_pledged_id shares_pledged]

getGithubIssues :: Project -> Handler [GH.Issue]
getGithubIssues project =
    getGithubIssues'
    >>= liftIO . wait
    >>= either (\_ -> addAlert "danger" "failed to fetch GitHub tickets\n" >> return [])
               return
  where
    getGithubIssues' :: Handler (Async (Either GH.Error [GH.Issue]))
    getGithubIssues' = liftIO . async $
        maybe
            (return $ Right [])
            (\(account, repo) -> GH.issuesForRepo account repo [])
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
    from $ \page -> do
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
        from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject ==. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

    getOtherTags :: DB [Entity Tag]
    getOtherTags =
        selectDistinct $
        from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject !=. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

-- | Get all of a Project's WikiPages, sorted alphabetically.
getProjectWikiPages :: ProjectId ->  DB [Entity WikiPage]
getProjectWikiPages project_id =
    select $
    from $ \ wp -> do
    where_ (exprWikiPageOnProject wp project_id)
    orderBy [asc (wp ^. WikiPageTarget)]
    return wp

-- | Fetch all Discussions that are somewhere on the given Project.
fetchProjectDiscussionsDB :: ProjectId -> DB [DiscussionId]
fetchProjectDiscussionsDB project_id = do
    pd <- projectDiscussion <$> getJust project_id
    wpds <- fmap (map unValue) $
                select $
                from $ \wp -> do
                where_ (wp ^. WikiPageProject ==. val project_id)
                return (wp ^. WikiPageDiscussion)
    return (pd : wpds)

-- | Get all (posted, not pending) Comments made *somewhere* on a Project, before the given time.
fetchProjectCommentsIncludingRethreadedBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [Entity Comment]
fetchProjectCommentsIncludingRethreadedBeforeDB project_id muser_id before lim = fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    select $
    from $ \(ecp `InnerJoin` c) -> do
    on_ (ecp ^. EventCommentPostedComment ==. c ^. CommentId)
    where_ $
        ecp ^. EventCommentPostedTs <=. val before &&.
        exprCommentProjectPermissionFilterIncludingRethreaded muser_id (val project_id) c &&.
        c ^. CommentDiscussion `in_` valList project_discussions
    orderBy [ desc $ ecp ^. EventCommentPostedTs, desc $ ecp ^. EventCommentPostedId ]
    limit lim
    return c

-- | Get all Rethreads whose *destinations* are on the given Project.
fetchProjectCommentRethreadsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [Entity Rethread]
fetchProjectCommentRethreadsBeforeDB project_id muser_id before lim = fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    select $
    from $ \(ecr `InnerJoin` r `InnerJoin` c) -> do
    on_ (r ^. RethreadNewComment ==. c ^. CommentId)
    on_ (ecr ^. EventCommentRethreadedRethread ==. r ^. RethreadId)
    where_ $
        ecr ^. EventCommentRethreadedTs <=. val before &&.
        exprCommentProjectPermissionFilter muser_id (val project_id) c &&.
        c ^. CommentDiscussion `in_` valList project_discussions
    orderBy [ desc $ ecr ^. EventCommentRethreadedTs, desc $ ecr ^. EventCommentRethreadedId ]
    limit lim
    return r

-- | Fetch all WikiPages made on this Project before this time.
fetchProjectWikiPagesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity WikiPage]
fetchProjectWikiPagesBeforeDB project_id before lim =
    select $
    from $ \(ewp `InnerJoin` wp) -> do
    on_ (ewp ^. EventWikiPageWikiPage ==. wp ^. WikiPageId)
    where_ $
        ewp ^. EventWikiPageTs <=. val before &&.
        exprWikiPageOnProject wp project_id
    orderBy [ desc $ ewp ^. EventWikiPageTs, desc $ ewp ^. EventWikiPageId ]
    limit lim
    return wp

-- | Fetch all BlogPosts made on this Project before this time.
fetchProjectBlogPostsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity BlogPost]
fetchProjectBlogPostsBeforeDB project_id before lim =
    select $
    from $ \(ebp `InnerJoin` bp) -> do
    on_ (ebp ^. EventBlogPostPost ==. bp ^. BlogPostId)
    where_ $
        ebp ^. EventBlogPostTs <=. val before &&.
        bp ^. BlogPostProject ==. val project_id
    orderBy [ desc $ ebp ^. EventBlogPostTs, desc $ ebp ^. EventBlogPostId ]
    limit lim
    return bp

-- | Fetch all WikiEdits made on this Project before this time.
fetchProjectWikiEditsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity WikiEdit]
fetchProjectWikiEditsBeforeDB project_id before lim =
    select $
    from $ \(ewe `InnerJoin` we `InnerJoin` wp) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    on_ (ewe ^. EventWikiEditWikiEdit ==. we ^. WikiEditId)
    where_ $
        ewe ^. EventWikiEditTs <=. val before &&.
        exprWikiPageOnProject wp project_id
    orderBy [ desc $ ewe ^. EventWikiEditTs, desc $ ewe ^. EventWikiEditId ]
    limit lim
    return we

-- | Fetch all new SharesPledged made on this Project before this time.
fetchProjectNewPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity SharesPledged]
fetchProjectNewPledgesBeforeDB project_id before lim =
    select $
    from $ \(enp `InnerJoin` sp) -> do
    on_ (enp ^. EventNewPledgeSharesPledged ==. sp ^. SharesPledgedId)
    where_ $
        enp ^. EventNewPledgeTs <=. val before &&.
        sp ^. SharesPledgedProject ==. val project_id
    orderBy [ desc $ enp ^. EventNewPledgeTs, desc $ enp ^. EventNewPledgeId ]
    limit lim
    return sp

-- | Fetch all updated Pledges made on this Project before this time, along with the old number of shares.
fetchProjectUpdatedPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(Int64, Entity SharesPledged)]
fetchProjectUpdatedPledgesBeforeDB project_id before lim = fmap (map (\(Value n, p) -> (n, p))) $
    select $
    from $ \(eup `InnerJoin` sp) -> do
    on_ (eup ^. EventUpdatedPledgeSharesPledged ==. sp ^. SharesPledgedId)
    where_ $
        eup ^. EventUpdatedPledgeTs <=. val before &&.
        sp ^. SharesPledgedProject ==. val project_id
    orderBy [ desc $ eup ^. EventUpdatedPledgeTs, desc $ eup ^. EventUpdatedPledgeId ]
    limit lim
    return (eup ^. EventUpdatedPledgeOldShares, sp)

-- | Fetch all deleted pledge events made on this Project before this time.
fetchProjectDeletedPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [EventDeletedPledge]
fetchProjectDeletedPledgesBeforeDB project_id before lim = fmap (map entityVal) $
    select $
    from $ \edp -> do
    where_ $
        edp ^. EventDeletedPledgeTs      <=. val before &&.
        edp ^. EventDeletedPledgeProject ==. val project_id
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
    from $ \pur -> do
    where_ $
        pur ^. ProjectUserRoleProject ==. val project_id &&.
        pur ^. ProjectUserRoleRole    ==. val role
    return (pur ^. ProjectUserRoleUser)
  --
-- | Fetch all Project VolunteerApplications.
fetchProjectVolunteerApplicationsDB :: ProjectId -> DB [Entity VolunteerApplication]
fetchProjectVolunteerApplicationsDB project_id =
    select $
    from $ \va -> do
    where_ (va ^. VolunteerApplicationProject ==. val project_id)
    orderBy [desc (va ^. VolunteerApplicationCreatedTs)]
    return va

-- | Fetch a WikiPage (maybe), given the Project handle and WikiPage target.
-- (Presumably, these Texts come from something like a rethread form,
-- where the user types in URLs manually).
fetchProjectWikiPageByNameDB :: Text -> Text -> DB (Maybe (Entity WikiPage))
fetchProjectWikiPageByNameDB project_handle target = runMaybeT $ do
    Entity project_id _ <- MaybeT (getBy (UniqueProjectHandle project_handle))
    MaybeT (getBy (UniqueWikiTarget project_id target))

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
        from $ \(t `InnerJoin` c) -> do
        on_ (t ^. TicketComment ==. c ^. CommentId)
        where_ $
            c ^. CommentDiscussion `in_` valList discussion_ids &&.
            exprCommentOpen c
        return t
