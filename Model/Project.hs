module Model.Project
    ( ProjectSummary(..)
    , fetchProjectCommentsPostedOnWikiPagesDB
    , fetchProjectWikiEditsDB
    -- TODO(mitchell): rename all these... prefix fetch, suffix DB
    , getAllProjects
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

import           Model.Currency

import           Control.Monad.Trans.Resource (MonadThrow)
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

getAllProjects :: DB [Entity Project]
getAllProjects =
    select $
    from $ \p ->
    return p

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
    where_ $ wp ^. WikiPageProject ==. val project_id
    orderBy [asc (wp ^. WikiPageTarget)]
    return wp

-- | Fetch all Comments posted on some Project's WikiPages.
fetchProjectCommentsPostedOnWikiPagesDB :: ProjectId -> UTCTime -> DB [(Entity Comment, Entity WikiPage)]
fetchProjectCommentsPostedOnWikiPagesDB project_id before =
    select $
    from $ \(ecp `InnerJoin` c `InnerJoin` wp) -> do
    on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
    on_ (ecp ^. EventCommentPostedComment ==. c ^. CommentId)
    where_ $
        ecp ^. EventCommentPostedTs <=. val before &&.
        wp ^. WikiPageProject ==. val project_id
    return (c, wp)

-- | Fetch all WikiEdits made on some Project.
fetchProjectWikiEditsDB :: ProjectId -> UTCTime -> DB [(Entity WikiEdit, Entity WikiPage)]
fetchProjectWikiEditsDB project_id before =
    select $
    from $ \(ewe `InnerJoin` we `InnerJoin` wp) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    on_ (ewe ^. EventWikiEditWikiEdit ==. we ^. WikiEditId)
    where_ $
        ewe ^. EventWikiEditTs <=. val before &&.
        wp ^. WikiPageProject ==. val project_id
    return (we, wp)
