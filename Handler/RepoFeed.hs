module Handler.RepoFeed where

import Import

import Data.Text.Encoding

import Yesod.Feed

import Data.Git.Storage
import Data.Git.Named
import Data.Git.Repository
import Data.Git.Types
import Data.Git.Ref

import qualified Data.Text as T

import Data.Text.PrettyHtml

import Prelude (head)

import Data.Time (addUTCTime) 
import Data.List (sortBy)

import Data.Tree (unfoldTreeM_BF, levels)


getRepoFeedR :: HasGithubRepo Handler => Handler TypedContent
getRepoFeedR = do
    now <- liftIO getCurrentTime
    let bound = addUTCTime (-2000000) now
    
    (commits, branch) <- liftIO $ do
        repo <- openRepo ".git"
        RefLink (RefBranch branch) <- readRefFile ".git" RefHead
        RefDirect ref <- readRefFile ".git" (RefBranch branch)
        commits <- getCommits repo ref bound
        return (commits, branch)


    let title = T.pack $ "Snowdrift Commits (" ++ branch ++ ")"
        feed_url = RepoFeedR
        home_url = HomeR
        author = "Snowdrift Team"
        description = "Commits to the Snowdrift repository."
        lang = "en"
        -- commitTime = toUTCTime . personTime . commitAuthor
        time = commitTime $ head commits

    entries <- forM commits $ \ commit -> do
            let ls = T.lines $ decodeUtf8 $ commitMessage commit
            html <- unlinesHtml <$> mapM (prettyHtml prettyThings) ls
            return $ FeedEntry (ProjectR "snowdrift") (commitTime commit) (fromMaybe "empty commit message" $ listToMaybe ls) html

    newsFeed $ Feed title feed_url home_url author description lang time entries


commitTime :: Commit -> UTCTime
commitTime = toUTCTime . personTime . commitAuthor

getCommits :: Git -> Ref -> UTCTime -> IO [Commit]
getCommits repo ref bound = do
        tree <- flip unfoldTreeM_BF ref $ \ ref' -> do
            commit <- getCommit repo ref'
            return $ if commitTime commit < bound
                      then (commit, [])
                      else (commit, commitParents commit)

        return $ sortBy (flip compare `on` commitTime) $ filter (\ a -> length (commitParents a) <= 1) $ concat $ levels tree

