module Model.Project where

import Import

import Model.Currency

import Database.Persist.GenericSql

import Data.Conduit (MonadUnsafeIO, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)


data ProjectSummary =
    ProjectSummary
        { summaryName :: Text
        , summaryProjectId :: ProjectId
        , summaryUsers :: UserCount
        , summaryShares :: ShareCount
        , summaryShareCost :: Milray
        }

summarizeProject :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) => (Entity Project, [Entity Pledge]) -> SqlPersist m ProjectSummary
summarizeProject (project, pledges) = do
    let share_value = projectShareValue $ entityVal project
        share_count = ShareCount $ sum . map (pledgeFundedShares . entityVal) $ pledges
        user_count = UserCount $ fromIntegral $ length pledges

    return $ ProjectSummary (projectName $ entityVal project) (entityKey project) user_count share_count share_value


getProjectShares :: (MonadUnsafeIO m, MonadThrow m, MonadIO m, MonadBaseControl IO m, MonadLogger m) => ProjectId -> SqlPersist m [Int64]
getProjectShares project_id = do
    pledges <- selectList [ PledgeProject ==. project_id, PledgeFundedShares >. 0 ] []
    return $ map (pledgeFundedShares . entityVal) pledges


projectComputeShareValue :: [Int64] -> Milray
projectComputeShareValue pledges =
    let lg x = logBase 2 x :: Double
        num_users = fromIntegral $ length pledges
        geomean :: [Double] -> Double
        geomean xs = exp $ sum (map log xs) / fromIntegral (length xs)
        multiplier = lg (geomean (map fromIntegral pledges) + 1)
     in Milray 1 $* (multiplier * (num_users - 1))


updateShareValue :: (MonadUnsafeIO m, MonadThrow m, MonadIO m, MonadBaseControl IO m, MonadLogger m) => ProjectId -> SqlPersist m ()
updateShareValue project_id = do
    pledges <- getProjectShares project_id
    
    update project_id [ ProjectShareValue =. projectComputeShareValue pledges ]

{-
 - TODO
 -  Unfund shares
 -  Fix algorithm
 -}

