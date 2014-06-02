module Model.Project where

import Import

import Model.Currency
import Model.ViewType
import Model.User

import Control.Monad.Trans.Resource

data ProjectSummary =
    ProjectSummary
        { summaryName :: Text
        , summaryProjectHandle :: Text
        , summaryUsers :: UserCount
        , summaryShares :: ShareCount
        , summaryShareCost :: Milray
        }

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


projectComputeShareValue :: [Int64] -> Milray
projectComputeShareValue pledges =
    let lg x = logBase 2 x :: Double
        num_users = fromIntegral $ length pledges
        geomean :: [Double] -> Double
        geomean xs = exp $ sum (map log xs) / fromIntegral (length xs)
        multiplier = lg (geomean (map fromIntegral pledges) * 2)
     in Milray 1 $* (multiplier * (num_users - 1))


updateShareValue :: (MonadThrow m, MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadResource m) => ProjectId -> SqlPersistT m ()
updateShareValue project_id = do
    pledges <- getProjectShares project_id
    
    update $ \ project -> do
        set project  [ ProjectShareValue =. val (projectComputeShareValue pledges) ]
        where_ (project ^. ProjectId ==. val project_id)

getCounts :: (MonadLogger m, MonadResource m, MonadIO m, MonadBaseControl IO m, MonadThrow m)
    => Entity User -> [Entity Project] -> SqlPersistT m [([Value Int], [Value Int])]

getCounts (Entity user_id user) = mapM $ \(Entity project_id project) -> do
    moderator <- isProjectModerator (projectHandle project) user_id

    comment_viewtimes :: [Entity ViewTime] <- select $ from $ \ viewtime -> do
        where_ $ 
            ( viewtime ^. ViewTimeUser ==. val user_id ) &&.
            ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
            ( viewtime ^. ViewTimeType ==. val ViewComments )
        return viewtime

    edit_viewtimes :: [Entity ViewTime] <- select $ from $ \ viewtime -> do
        where_ $
            ( viewtime ^. ViewTimeUser ==. val user_id ) &&.
            ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
            ( viewtime ^. ViewTimeType ==. val ViewEdits )
        return viewtime

    let comments_ts = case comment_viewtimes of
            [] -> userReadComments user
            Entity _ viewtime : _ -> viewTimeTime viewtime
        edits_ts = case edit_viewtimes of
            [] -> userReadEdits user
            Entity _ viewtime : _ -> viewTimeTime viewtime

    comments <- select $ from $ \(comment `LeftOuterJoin` wp) -> do
        on_ $ wp ^. WikiPageDiscussion ==. comment ^. CommentDiscussion
        where_ $ foldl1 (&&.) $ catMaybes
            [ Just $ comment ^. CommentCreatedTs >=. val comments_ts
            , Just $ wp ^. WikiPageProject ==. val project_id
            , Just $ comment ^. CommentUser !=. val user_id
            , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
            ]
        return (countRows :: SqlExpr (Value Int))

    edits <- select $ from $ \(edit `LeftOuterJoin` wp) -> do
        on_ (wp ^. WikiPageId ==. edit ^. WikiEditPage)
        where_ $
            ( edit ^. WikiEditTs >=. val edits_ts ) &&.
            ( wp ^. WikiPageProject ==. val project_id ) &&.
            ( edit ^. WikiEditUser !=. val user_id )
        return (countRows :: SqlExpr (Value Int))

    return (comments, edits)

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

