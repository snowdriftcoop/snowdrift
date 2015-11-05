module Model.Project
    ( ProjectSummary(..)
    , UpdateProject(..)
    , fetchPublicProjectsDB
    , fetchProjectDB
    , fetchProjectCommentRethreadEventsBeforeDB
    , fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB
    , fetchProjectCommentClosingEventsBeforeDB
    , fetchProjectDiscussionsDB
    , fetchProjectModeratorsDB
    , fetchProjectTeamMembersDB
    , fetchProjectOpenTicketsDB
    , fetchProjectVolunteerApplicationsDB
    , fetchProjectWikiEditEventsWithTargetsBeforeDB
    , fetchProjectWikiPageEventsWithTargetsBeforeDB
    , fetchProjectBlogPostEventsBeforeDB
    , fetchProjectTicketClaimingEventsBeforeDB
    , fetchProjectTicketUnclaimingEventsBeforeDB
    , fetchProjectWikiPageByNameDB
    , fetchProjectPledgesDB
    , getGithubIssues
    , getProjectPages
    , getProjectTagList
    , getProjectWikiPages
    , projectNameWidget
    , summarizeProject
    -- * Balancing/deactivating pledges
    ) where

import Import

import Control.Concurrent.Async (Async, async, wait)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Github.Data as GH
import qualified Github.Issues as GH

import Data.Filter
import Data.Order
import Handler.Utils
import Model.Comment
import Model.Comment.Sql
import Model.Count
import Model.Issue
import Model.Tag
import Model.Wiki.Sql
import Widgets.Tag
import WrappedValues
import qualified Mechanism as Mech

--------------------------------------------------------------------------------
-- Types

newtype UserCount = UserCount Int64 deriving Count
newtype ShareCount = ShareCount Int64 deriving Count
newtype DiscussionCount = DiscussionCount Int64 deriving Count
newtype TicketCount = TicketCount Int64 deriving Count

data ProjectSummary = ProjectSummary
    { summaryName            :: Text
    , summaryProjectHandle   :: Text
    , summaryMech            :: Mech.Project
    , summaryDiscussionCount :: DiscussionCount
    , summaryTicketCount     :: TicketCount
    }

data UpdateProject = UpdateProject
    { updateProjectName        :: Text
    , updateProjectBlurb       :: Text
    , updateProjectDescription :: Markdown
    , updateProjectTags        :: [Text]
    , updateProjectGithubRepo  :: Maybe Text
    , updateProjectLogo        :: Maybe Text
    } deriving Show

newtype TaggedTicket = TaggedTicket ( (Entity Ticket)
                                    , Bool  -- claimed?
                                    , [AnnotatedTag] )

instance Issue TaggedTicket where
    issueWidget (TaggedTicket ((Entity ticket_id ticket),_,tags)) =
        [whamlet|
          <tr>
            <td>
              <a href=@{CommentDirectLinkR (ticketComment ticket)}>
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
ticketToFilterable (TaggedTicket (Entity _ ticket, is_claimed, tags)) =
    Filterable isClaimed has_tag get_named_ts search_literal
  where
    isClaimed "CLAIMED"   = is_claimed
    isClaimed "UNCLAIMED" = is_claimed  -- inverted in 'Data.Filter'
    isClaimed cmd         = error $ "Unrecognized command " <> T.unpack cmd

    has_tag t =
        any (\tag -> annotTagName tag == t && annotTagScore tag > 0) tags

    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)

ticketToOrderable :: TaggedTicket -> Orderable
ticketToOrderable (TaggedTicket ((Entity _ ticket), is_claimed, tags)) =
    Orderable isClaimed has_tag get_named_ts search_literal
  where
    isClaimed "CLAIMED"   = is_claimed
    isClaimed "UNCLAIMED" = is_claimed  -- inverted in 'Data.Order'
    isClaimed cmd         = error $ "Unrecognized command " <> T.unpack cmd

    has_tag t = elem t $ map annotTagName tags

    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)

--------------------------------------------------------------------------------
-- Database actions

fetchPublicProjectsDB :: DB [Entity Project]
fetchPublicProjectsDB = select $ from $ \p -> do
    where_ $ p ^. ProjectPublic
    return p

fetchProjectDB :: ProjectId -> DB [Entity Project]
fetchProjectDB project_id =
    select $ from $ \p -> do
        where_ $ p ^. ProjectId ==. val project_id
        return p

getGithubIssues :: Project -> Handler [GH.Issue]
getGithubIssues project =
    getGithubIssues'
    >>= liftIO . wait
    >>= either (\_ -> addAlert "danger" eMsg >> return [])
               return
  where
    eMsg = "failed to fetch GitHub tickets\n"
    getGithubIssues' :: Handler (Async (Either GH.Error [GH.Issue]))
    getGithubIssues' = liftIO . async $
        maybe
            (return $ Right [])
            (\(account, repo) -> GH.issuesForRepo account repo [])
            parsedProjectGithubRepo

    parsedProjectGithubRepo :: Maybe (String, String)
    parsedProjectGithubRepo =
        fmap (second (drop 1) . break (== '/') . T.unpack)
             (projectGithubRepo project)

summarizeProject :: Entity Project
                 -> Mech.Project
                 -> [DiscussionId]
                 -> [TaggedTicket]
                 -> ProjectSummary
summarizeProject project _mechProj discussions tickets =
    let discussion_count = DiscussionCount $
            fromIntegral $ length discussions
        ticket_count = TicketCount $ fromIntegral $ length tickets
    in ProjectSummary
        (projectName $ entityVal project)
        (projectHandle $ entityVal project)
        Mech.Project
        discussion_count
        ticket_count

fetchProjectPledgesDB :: ( MonadThrow m
                         , MonadIO m
                         , MonadBaseControl IO m
                         , MonadLogger m
                         , MonadResource m)
                      => ProjectId
                      -> SqlPersistT m [Entity Pledge]
fetchProjectPledgesDB project_id = do
    pledges <- select $ from $ \pledge -> do
        where_
            (pledge ^. PledgeProject ==. val project_id
             &&. pledge ^. PledgeFundedShares >. val 0)
        return pledge

    return pledges

-- | Get all WikiPages for a Project.
getProjectPages :: ProjectId -> DB [Entity WikiPage]
getProjectPages project_id =
    select $
    from $ \page -> do
    where_ $ page ^. WikiPageProject ==. val project_id
    return page

projectNameWidget :: ProjectId -> Widget
projectNameWidget project_id = do
    maybe_project <- handlerToWidget $ runDB $ get project_id
    case maybe_project of
        Nothing -> [whamlet| (unknown project) |]
        Just project -> [whamlet|
                            <a href=@{ProjectR $ projectHandle project}>
                              #{projectName project}
                        |]

getProjectTagList :: ProjectId -> DB ([Entity Tag], [Entity Tag])
getProjectTagList project_id = (,) <$> getProjectTags <*> getOtherTags
  where
    getProjectTags :: DB [Entity Tag]
    getProjectTags =
        select . distinct $
        from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ $ page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion
        on_ $ comment ^. CommentId ==. rel ^. CommentTagComment
        on_ $ rel ^. CommentTagTag ==. tag ^. TagId

        where_ $
            page ^. WikiPageProject ==. val project_id
            &&. tag ^. TagId `notIn` deprecatedTags
        orderBy [ desc (tag ^. TagName) ]
        return tag

    deprecatedTags = subList_select $ from $ \dt -> do
        where_ $ dt ^. DeprecatedTagProject ==. val project_id
        return $ dt ^. DeprecatedTagTag

    getOtherTags :: DB [Entity Tag]
    getOtherTags =
        select . distinct $
        from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ $ page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion
        on_ $ comment ^. CommentId ==. rel ^. CommentTagComment
        on_ $ rel ^. CommentTagTag ==. tag ^. TagId
        where_ $ page ^. WikiPageProject !=. val project_id
        orderBy [ desc (tag ^. TagName) ]
        return tag

-- | Get all of a Project's WikiPages, sorted alphabetically.
getProjectWikiPages :: [Language] -> ProjectId ->  DB [Entity WikiTarget]
getProjectWikiPages languages project_id = do
    targets <- select $ from $ \wt -> do
        where_ $ wt ^. WikiTargetProject ==. val project_id
        return wt

    return $ sortBy (compare `on` (wikiTargetTarget . entityVal))
                    (pickTargetsByLanguage languages targets)

-- | Fetch all Discussions that are somewhere on the given Project.
fetchProjectDiscussionsDB :: ProjectId -> DB [DiscussionId]
fetchProjectDiscussionsDB project_id = do
    pd <- projectDiscussion <$> getJust project_id

    wpds <- fmap unwrapValues $ select $ from $ \wp -> do
        where_ $ wp ^. WikiPageProject ==. val project_id
        return $ wp ^. WikiPageDiscussion

    bpds <- fmap unwrapValues $ select $ from $ \bp -> do
        where_ $ bp ^. BlogPostProject ==. val project_id
        return $ bp ^. BlogPostDiscussion

    return (pd : wpds ++ bpds)

-- | Get all (posted, not pending) Comments made *somewhere* on a Project,
-- before the given time.
fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB
    :: ProjectId
    -> Maybe UserId
    -> UTCTime
    -> Int64
    -> DB [(EventCommentPostedId, Entity Comment)]
fetchProjectCommentPostedEventsIncludingRethreadedBeforeDB
        project_id
        muser_id
        before lim =

    fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    fmap unwrapValues $
        select $
        from $ \(ecp `InnerJoin` c) -> do
        on_ $ ecp ^. EventCommentPostedComment ==. c ^. CommentId
        where_ $
            ecp ^. EventCommentPostedTs <=. val before
            &&. exprCommentProjectPermissionFilterIncludingRethreaded
                muser_id
                (val project_id)
                c
            &&. c ^. CommentDiscussion `in_` valList project_discussions
        orderBy
            [ desc $ ecp ^. EventCommentPostedTs
            , desc $ ecp ^. EventCommentPostedId ]
        limit lim
        return (ecp ^. EventCommentPostedId, c)

-- | Get all Rethreads whose *destinations* are on the given Project.
fetchProjectCommentRethreadEventsBeforeDB
    :: ProjectId
    -> Maybe UserId
    -> UTCTime
    -> Int64
    -> DB [(EventCommentRethreadedId, Entity Rethread)]
fetchProjectCommentRethreadEventsBeforeDB project_id muser_id before lim =
    fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    fmap unwrapValues $
        select $
        from $ \(ecr `InnerJoin` r `InnerJoin` c) -> do
        on_ $ r ^. RethreadNewComment ==. c ^. CommentId
        on_ $ ecr ^. EventCommentRethreadedRethread ==. r ^. RethreadId
        where_ $ ecr ^. EventCommentRethreadedTs <=. val before
            &&. exprCommentProjectPermissionFilter muser_id (val project_id) c
            &&. c ^. CommentDiscussion `in_` valList project_discussions
        orderBy
            [ desc $ ecr ^. EventCommentRethreadedTs
            , desc $ ecr ^. EventCommentRethreadedId ]
        limit lim
        return (ecr ^. EventCommentRethreadedId, r)

-- | Get all Closings for comments on the current project
fetchProjectCommentClosingEventsBeforeDB
    :: ProjectId
    -> Maybe UserId
    -> UTCTime
    -> Int64
    -> DB [(EventCommentClosingId, Entity CommentClosing)]
fetchProjectCommentClosingEventsBeforeDB project_id muser_id before lim =
    fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    fmap unwrapValues $
        select $ from $ \(ecc `InnerJoin` cc `InnerJoin` c) -> do
        on_ $ cc ^. CommentClosingComment ==. c ^. CommentId
        on_ $
            ecc ^. EventCommentClosingCommentClosing ==. cc ^. CommentClosingId
        where_ $
            ecc ^. EventCommentClosingTs <=. val before
            &&. exprCommentProjectPermissionFilter muser_id (val project_id) c
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy
            [ desc $ cc ^. CommentClosingTs
            , desc $ cc ^. CommentClosingId ]
        limit lim
        return (ecc ^. EventCommentClosingId, cc)

fetchProjectTicketClaimingEventsBeforeDB
    :: ProjectId
    -> UTCTime
    -> Int64
    -> DB [( EventTicketClaimedId
           , Either (Entity TicketClaiming)
                    (Entity TicketOldClaiming))]
fetchProjectTicketClaimingEventsBeforeDB project_id before lim = do
    project_discussions <- fetchProjectDiscussionsDB project_id

    tuples <- fmap unwrapValues $
        select $
        from $ \(etc
                 `LeftOuterJoin` tc
                 `LeftOuterJoin` toc
                 `LeftOuterJoin` c
                 `InnerJoin`     t) -> do
        on_ $ t ^. TicketComment ==. c ^. CommentId
        on_ $
            just (c ^. CommentId) ==. tc ?. TicketClaimingTicket
            ||. just (c ^. CommentId) ==. toc ?. TicketOldClaimingTicket
        on_ $ etc ^. EventTicketClaimedOldClaim ==. toc ?. TicketOldClaimingId
        on_ $ etc ^. EventTicketClaimedClaim ==. tc ?. TicketClaimingId

        where_ $
            etc ^. EventTicketClaimedTs <=. val before
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy [ desc $ etc ^. EventTicketClaimedTs
                , desc $ etc ^. EventTicketClaimedId ]
        limit lim
        return (etc ^. EventTicketClaimedId, tc, toc)

    let tupleToMaybeEither (etc_id, Just x, Nothing) = Just $ (etc_id, Left x)
        tupleToMaybeEither (etc_id, Nothing, Just y) = Just $ (etc_id, Right y)
        tupleToMaybeEither _ = Nothing

    return $ mapMaybe tupleToMaybeEither tuples

fetchProjectTicketUnclaimingEventsBeforeDB
    :: ProjectId
    -> UTCTime
    -> Int64
    -> DB [(EventTicketUnclaimedId, Entity TicketOldClaiming)]
fetchProjectTicketUnclaimingEventsBeforeDB project_id before lim =
    fetchProjectDiscussionsDB project_id >>= \project_discussions ->
    fmap unwrapValues $
        select $
        from $ \(etu `InnerJoin` toc `InnerJoin` c `InnerJoin` t) -> do
        on_ $ t ^. TicketComment ==. c ^. CommentId
        on_ $ toc ^. TicketOldClaimingTicket ==. c ^. CommentId
        on_ $ etu ^. EventTicketUnclaimedClaim ==. toc ^. TicketOldClaimingId

        where_ $ etu ^. EventTicketUnclaimedTs <=. val before
            &&. c ^. CommentDiscussion `in_` valList project_discussions

        orderBy [ desc $ etu ^. EventTicketUnclaimedTs
                , desc $ etu ^. EventTicketUnclaimedId ]
        limit lim
        return (etu ^. EventTicketUnclaimedId, toc)

-- | Fetch all WikiPages made on this Project before this time.
fetchProjectWikiPageEventsWithTargetsBeforeDB
    :: [Language]
    -> ProjectId
    -> UTCTime
    -> Int64
    -> DB [(EventWikiPageId, Entity WikiPage, WikiTarget)]
fetchProjectWikiPageEventsWithTargetsBeforeDB
        languages
        project_id
        before
        lim = do

    pages <- fmap unwrapValues $ select $ from $ \(ewp `InnerJoin` wp) -> do
        on_ (ewp ^. EventWikiPageWikiPage ==. wp ^. WikiPageId)
        where_ $ ewp ^. EventWikiPageTs <=. val before
            &&. exprWikiPageOnProject wp project_id
        orderBy [ desc $ ewp ^. EventWikiPageTs
                , desc $ ewp ^. EventWikiPageId ]
        limit lim
        return (ewp ^. EventWikiPageId, wp)

    targets <- select $ from $ \wt -> do
        where_ $
            wt ^. WikiTargetPage `in_` valList (map (entityKey . snd) pages)
        return wt


    let events = map (second entityKey) pages
        pages_map = M.fromList $ map ((entityKey &&& id) . snd) pages
        targets_map =
            M.fromList $
                map (\(Entity _ wiki_target) ->
                        (wikiTargetPage wiki_target, wiki_target))
                    (pickTargetsByLanguage languages targets)

    return $
        mapMaybe
            (\(ewp_id, page_id) ->
                (ewp_id,,)
                    <$> M.lookup page_id pages_map
                    <*> M.lookup page_id targets_map)
            events

-- | Fetch all BlogPosts made on this Project before this time.
fetchProjectBlogPostEventsBeforeDB
    :: ProjectId
    -> UTCTime
    -> Int64
    -> DB [(EventBlogPostId, Entity BlogPost)]
fetchProjectBlogPostEventsBeforeDB project_id before lim =
    fmap unwrapValues $ select $ from $ \(ebp `InnerJoin` bp) -> do
        on_ $ ebp ^. EventBlogPostPost ==. bp ^. BlogPostId
        where_ $ ebp ^. EventBlogPostTs <=. val before
            &&. bp ^. BlogPostProject ==. val project_id

        orderBy [ desc $ ebp ^. EventBlogPostTs, desc $ ebp ^. EventBlogPostId ]
        limit lim
        return (ebp ^. EventBlogPostId, bp)

-- | Fetch all WikiEdits made on this Project before this time.
fetchProjectWikiEditEventsWithTargetsBeforeDB
    :: [Language]
    -> ProjectId
    -> UTCTime
    -> Int64
    -> DB [(EventWikiEditId, Entity WikiEdit, WikiTarget)]
fetchProjectWikiEditEventsWithTargetsBeforeDB
        languages
        project_id
        before
        lim = do

    edits <- fmap unwrapValues $
        select $
        from $ \(ewe `InnerJoin` we `InnerJoin` wp) -> do
        on_ $ wp ^. WikiPageId ==. we ^. WikiEditPage
        on_ $ ewe ^. EventWikiEditWikiEdit ==. we ^. WikiEditId

        where_ $ ewe ^. EventWikiEditTs <=. val before
            &&.  exprWikiPageOnProject wp project_id

        orderBy [ desc $ ewe ^. EventWikiEditTs, desc $ ewe ^. EventWikiEditId ]
        limit lim
        return (ewe ^. EventWikiEditId, we)

    targets <- select $ from $ \wt -> do
        where_ $
            wt ^. WikiTargetPage
                `in_` valList (map (wikiEditPage . entityVal . snd) edits)
        return wt

    let events = map (id *** (entityKey &&& wikiEditPage . entityVal)) edits
        edits_map = M.fromList $ map ((entityKey &&& id) . snd) edits
        targets_map =
            M.fromList $
                map ((wikiTargetPage &&& id) . entityVal)
                    (pickTargetsByLanguage languages targets)

    return $
        mapMaybe
            (\(ewe_id, (edit_id, page_id)) ->
                (ewe_id,,)
                    <$> M.lookup edit_id edits_map
                    <*> M.lookup page_id targets_map)
            events

-- | Fetch this Project's team members.
fetchProjectTeamMembersDB :: ProjectId -> DB [UserId]
fetchProjectTeamMembersDB = fetchProjectRoleDB TeamMember

fetchProjectModeratorsDB :: ProjectId -> DB [UserId]
fetchProjectModeratorsDB = fetchProjectRoleDB Moderator

-- | Abstract fetching Project Admins, TeamMembers, etc. Not exported.
fetchProjectRoleDB :: Role -> ProjectId -> DB [UserId]
fetchProjectRoleDB role project_id = fmap unwrapValues $
    select $
    from $ \pur -> do
    where_ $
        pur ^. ProjectUserRoleProject ==. val project_id &&.
        pur ^. ProjectUserRoleRole    ==. val role
    return (pur ^. ProjectUserRoleUser)
  --
-- | Fetch all Project VolunteerApplications.
fetchProjectVolunteerApplicationsDB
    :: ProjectId
    -> DB [Entity VolunteerApplication]
fetchProjectVolunteerApplicationsDB project_id =
    select $
    from $ \va -> do
    where_ (va ^. VolunteerApplicationProject ==. val project_id)
    orderBy [desc (va ^. VolunteerApplicationCreatedTs)]
    return va

-- | Fetch a WikiPage (maybe), given the Project handle and WikiPage target.
-- (Presumably, these Texts come from something like a rethread form,
-- where the user types in URLs manually).
fetchProjectWikiPageByNameDB
    :: Text
    -> Language
    -> Text
    -> DB (Maybe (Entity WikiPage))
fetchProjectWikiPageByNameDB project_handle language target = runMaybeT $ do
    Entity project_id _ <- MaybeT $ getBy $ UniqueProjectHandle project_handle
    Entity _ wiki_target <-
        MaybeT $ getBy $ UniqueWikiTarget project_id language target
    wiki_page <- MaybeT $ get $ wikiTargetPage wiki_target
    return $ Entity (wikiTargetPage wiki_target) wiki_page

fetchProjectOpenTicketsDB :: ProjectId -> Maybe UserId -> DB [TaggedTicket]
fetchProjectOpenTicketsDB project_id muser_id = do
    tickets <- fetchProjectDiscussionsDB project_id >>= fetch_tickets
    annot_tags_map <-
        fetchCommentCommentTagsInDB
            (map (ticketComment . entityVal . fst) tickets)
        >>= buildAnnotatedCommentTagsDB muser_id
    let tagTicket :: (Entity Ticket, Bool) -> TaggedTicket
        tagTicket (t@(Entity _ ticket),is_claimed) =
            TaggedTicket
                ( t
                , is_claimed
                , M.findWithDefault [] (ticketComment ticket) annot_tags_map)
    return (map tagTicket tickets)
  where
    fetch_tickets :: [DiscussionId] -> DB [(Entity Ticket, Bool)]
    fetch_tickets discussion_ids = do
        ts <- select $
              from $ \(t `InnerJoin` c) -> do
              on_ (t ^. TicketComment ==. c ^. CommentId)
              where_ $
                  c ^. CommentDiscussion `in_` valList discussion_ids &&.
                  exprCommentOpen c &&.
                  c ^. CommentId `notIn`
                      (subList_select $
                       from $
                       return . (^. CommentRethreadOldComment))
              return t
        forM ts $ \t@(Entity _ ticket) -> do
            c <- P.count [TicketClaimingTicket P.==. ticketComment ticket]
            return $ (t, c /= 0)
