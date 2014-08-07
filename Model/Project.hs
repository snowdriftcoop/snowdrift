module Model.Project
    ( ProjectSummary(..)
    , fetchAllProjectsDB
    , fetchProjectCommentIdsDB
    , fetchProjectCommentsPendingBeforeDB
    , fetchProjectCommentsPostedOnWikiPagesBeforeDB
    , fetchProjectDeletedPledgesBeforeDB
    , fetchProjectNewPledgesBeforeDB
    , fetchProjectTeamMembersDB
    , fetchProjectUpdatedPledgesBeforeDB
    , fetchProjectWikiEditsBeforeDB
    , fetchProjectWikiPagesBeforeDB
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

import           Model.Comment.Sql
import           Model.Currency
import           Model.Project.Sql
import           Model.WikiPage.Sql

import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Writer.Strict  (tell)
import           Control.Concurrent.Async     (Async, async, wait)
import qualified Github.Data                  as GH
import qualified Github.Issues                as GH
import qualified Data.Text                    as T

data ProjectSummary =
    ProjectSummary
        { summaryName          :: Text
        , summaryProjectHandle :: Text
        , summaryUsers         :: UserCount
        , summaryShares        :: ShareCount
        , summaryShareCost     :: Milray
        }

-- | Fetch all Comments made on this Project, somewhere (for now, just WikiPages)
fetchProjectCommentIdsDB :: ProjectId -> DB [CommentId]
fetchProjectCommentIdsDB = fetchProjectCommentIdsPostedOnWikiPagesDB

fetchAllProjectsDB :: DB [Entity Project]
fetchAllProjectsDB = select (from return)

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

-- | Fetch all Comments posted on this Project's WikiPages before this time.
fetchProjectCommentsPostedOnWikiPagesBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> DB [Entity Comment]
fetchProjectCommentsPostedOnWikiPagesBeforeDB project_id muser_id before =
    select $
    from $ \(ecp `InnerJoin` c `InnerJoin` wp) -> do
    on_ (exprCommentOnWikiPage c wp)
    on_ (ecp ^. EventCommentPostedComment ==. c ^. CommentId)
    where_ $
        ecp ^. EventCommentPostedTs <=. val before &&.
        exprWikiPageOnProject wp project_id &&.
        exprPermissionFilter muser_id (val project_id) c
    return c

-- | Fetch all CommentIds on this Project's WikiPages.
fetchProjectCommentIdsPostedOnWikiPagesDB :: ProjectId -> DB [CommentId]
fetchProjectCommentIdsPostedOnWikiPagesDB = fmap (map unValue) . select . querProjectCommentIdsPostedOnWikiPagesDB

-- | Fetch all pending Comments made on a Project before this time.
fetchProjectCommentsPendingBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> DB [Entity Comment]
fetchProjectCommentsPendingBeforeDB project_id muser_id before =
    select $
    from $ \(ecp `InnerJoin` c) -> do
    on_ (ecp ^. EventCommentPendingComment ==. c ^. CommentId)
    where_ $
        ecp ^. EventCommentPendingTs <=. val before &&.
        exprPermissionFilter muser_id (val project_id) c
    return c

-- | Fetch all WikiPages made on this Project before this time.
fetchProjectWikiPagesBeforeDB :: ProjectId -> UTCTime -> DB [Entity WikiPage]
fetchProjectWikiPagesBeforeDB project_id before =
    select $
    from $ \(ewp `InnerJoin` wp) -> do
    on_ (ewp ^. EventWikiPageWikiPage ==. wp ^. WikiPageId)
    where_ $
        ewp ^. EventWikiPageTs <=. val before &&.
        exprWikiPageOnProject wp project_id
    return wp

-- | Fetch all WikiEdits made on this Project before this time.
fetchProjectWikiEditsBeforeDB :: ProjectId -> UTCTime -> DB [Entity WikiEdit]
fetchProjectWikiEditsBeforeDB project_id before =
    select $
    from $ \(ewe `InnerJoin` we `InnerJoin` wp) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    on_ (ewe ^. EventWikiEditWikiEdit ==. we ^. WikiEditId)
    where_ $
        ewe ^. EventWikiEditTs <=. val before &&.
        exprWikiPageOnProject wp project_id
    return we

-- | Fetch all new SharesPledged made on this Project before this time.
fetchProjectNewPledgesBeforeDB :: ProjectId -> UTCTime -> DB [Entity SharesPledged]
fetchProjectNewPledgesBeforeDB project_id before =
    select $
    from $ \(enp `InnerJoin` sp) -> do
    on_ (enp ^. EventNewPledgeSharesPledged ==. sp ^. SharesPledgedId)
    where_ $
        enp ^. EventNewPledgeTs <=. val before &&.
        sp ^. SharesPledgedProject ==. val project_id
    return sp

-- | Fetch all updated Pledges made on this Project before this time, along with the old number of shares.
fetchProjectUpdatedPledgesBeforeDB :: ProjectId -> UTCTime -> DB [(Int64, Entity SharesPledged)]
fetchProjectUpdatedPledgesBeforeDB project_id before = fmap (map (\(Value n, p) -> (n, p))) $
    select $
    from $ \(eup `InnerJoin` sp) -> do
    on_ (eup ^. EventUpdatedPledgeSharesPledged ==. sp ^. SharesPledgedId)
    where_ $
        eup ^. EventUpdatedPledgeTs <=. val before &&.
        sp ^. SharesPledgedProject ==. val project_id
    return (eup ^. EventUpdatedPledgeOldShares, sp)

-- | Fetch all deleted pledge events made on this Project before this time.
fetchProjectDeletedPledgesBeforeDB :: ProjectId -> UTCTime -> DB [EventDeletedPledge]
fetchProjectDeletedPledgesBeforeDB project_id before = fmap (map entityVal) $
    select $
    from $ \edp -> do
    where_ $
        edp ^. EventDeletedPledgeTs      <=. val before &&.
        edp ^. EventDeletedPledgeProject ==. val project_id
    return edp

-- | Fetch this Project's team members.
fetchProjectTeamMembersDB :: ProjectId -> DB [UserId]
fetchProjectTeamMembersDB = fetchProjectRoleDB TeamMember

-- | Abstract fetching Project Admins, TeamMembers, etc. Not exported.
fetchProjectRoleDB :: Role -> ProjectId -> DB [UserId]
fetchProjectRoleDB role project_id = fmap (map unValue) $
    select $
    from $ \pur -> do
    where_ $
        pur ^. ProjectUserRoleProject ==. val project_id &&.
        pur ^. ProjectUserRoleRole    ==. val role
    return (pur ^. ProjectUserRoleUser)
