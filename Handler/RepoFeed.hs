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

import Data.Time (addUTCTime, secondsToDiffTime, UTCTime(..), fromGregorian)
import Data.Hourglass ( timeGetDate, dateYear, dateMonth, dateDay
                      , timeGetTimeOfDay, todHour, todMin, todSec, toSeconds )
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


    let title = T.pack $ "Snowdrift.coop Commits (" ++ refNameRaw branch ++ ")"
        feed_url = RepoFeedR
        home_url = HomeR
        author = "Snowdrift.coop Team"
        description = "Commits to the Snowdrift.coop repository."
        lang = "en"
        time = commitTime $ head commits

    entries <- forM commits $ \commit -> do
            let ls = T.lines $ decodeUtf8 $ commitMessage commit
            html <- unlinesHtml <$> mapM (prettyHtml prettyThings) ls
            return $ FeedEntry (ProjectR "snowdrift") (commitTime commit) (fromMaybe "empty commit message" $ listToMaybe ls) html

    newsFeed $ Feed title feed_url home_url author description lang time entries

-- Remove as soon as it's available in hit:
-- https://github.com/vincenthz/hit/pull/20
gitTimeToUTC :: GitTime -> UTCTime
gitTimeToUTC gt = UTCTime utcDay diffTime
    where
      date     = timeGetDate gt
      year     = toInteger $ dateYear date
      month    = succ $ fromEnum $ dateMonth date
      day      = dateDay date
      utcDay   = fromGregorian year month day
      tod      = timeGetTimeOfDay gt
      hours    = toInteger $ toSeconds $ todHour tod
      minutes  = toInteger $ toSeconds $ todMin tod
      seconds  = toInteger $ todSec tod
      diffTime = secondsToDiffTime $ hours + minutes + seconds

commitTime :: Commit -> UTCTime
commitTime = gitTimeToUTC . personTime . commitAuthor

getCommits :: Git -> Ref -> UTCTime -> IO [Commit]
getCommits repo ref bound = do
        tree <- flip unfoldTreeM_BF ref $ \ref' -> do
            commit <- getCommit repo ref'
            return $ if commitTime commit < bound
                      then (commit, [])
                      else (commit, commitParents commit)

        return $ sortBy (flip compare `on` commitTime) $ filter (\a -> length (commitParents a) <= 1) $ concat $ levels tree

