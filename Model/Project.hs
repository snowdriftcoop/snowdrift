module Model.Project
    ( ProjectSummary(..)
    , fetchAllProjectsDB
    , fetchProjectCommentIdsDB
    , fetchProjectCommentsPendingBeforeDB
    , fetchProjectCommentsPostedOnWikiPagesBeforeDB
    , fetchProjectWikiEditsBeforeDB
    , fetchProjectWikiPagesBeforeDB
    , fetchProjectNewPledgesBeforeDB
    , fetchProjectUpdatedPledgesBeforeDB
    , fetchProjectDeletedPledgesBeforeDB
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
    lift $ insert_ (SharesPledged now user_id shares pledge_render_id)
    let pledge = Pledge now user_id project_id shares shares
    insertBy pledge  >>= \case
        Left (Entity pledge_id _) -> do
            if shares == 0
                then do
                    lift (deleteKey pledge_id)
                    tell [EDeletedPledge now user_id project_id shares]
                else do
                    lift $
                        update $ \p -> do
                        set p [ PledgeShares       =. val shares
                              , PledgeFundedShares =. val shares
                              ]
                        where_ (p ^. PledgeId ==. val pledge_id)
                    tell [EUpdatedPledge shares pledge_id pledge]
        Right pledge_id -> tell [ENewPledge pledge_id pledge]

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

-- | Fetch all new Pledges made on this Project before this time.
fetchProjectNewPledgesBeforeDB :: ProjectId -> UTCTime -> DB [Entity Pledge]
fetchProjectNewPledgesBeforeDB project_id before =
    select $
    from $ \(enp `InnerJoin` p) -> do
    on_ (enp ^. EventNewPledgePledge ==. p ^. PledgeId)
    where_ $
        enp ^. EventNewPledgeTs <=. val before &&.
        p ^. PledgeProject ==. val project_id
    return p

-- | Fetch all updated Pledges made on this Project before this time, along with the old number of shares.
fetchProjectUpdatedPledgesBeforeDB :: ProjectId -> UTCTime -> DB [(Int64, Entity Pledge)]
fetchProjectUpdatedPledgesBeforeDB project_id before = fmap (map (\(Value n, p) -> (n, p))) $
    select $
    from $ \(eup `InnerJoin` p) -> do
    on_ (eup ^. EventUpdatedPledgePledge ==. p ^. PledgeId)
    where_ $
        eup ^. EventUpdatedPledgeTs <=. val before &&.
        p ^. PledgeProject ==. val project_id
    return (eup ^. EventUpdatedPledgeOldShares, p)

-- | Fetch all deleted pledge events made on this Project before this time.
fetchProjectDeletedPledgesBeforeDB :: ProjectId -> UTCTime -> DB [EventDeletedPledge]
fetchProjectDeletedPledgesBeforeDB project_id before = fmap (map entityVal) $
    select $
    from $ \edp -> do
    where_ $
        edp ^. EventDeletedPledgeTs      <=. val before &&.
        edp ^. EventDeletedPledgeProject ==. val project_id
    return edp
