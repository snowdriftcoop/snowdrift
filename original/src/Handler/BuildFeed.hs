-- This file generates https://snowdrift.coop/dev/build which is a feed showing updates to the live Snowdrift.coop site

module Handler.BuildFeed where

import Import

import Text.Blaze.Html5 (br)
import Yesod.Feed
import qualified Data.Text as T

getBuildFeedR :: Handler TypedContent
getBuildFeedR = do
    builds :: [Build] <- fmap (map entityVal) $ runDB $ selectList [] [Desc BuildBootTime]

    let title = T.pack "Snowdrift.coop Deployments"
        feed_url = BuildFeedR
        home_url = HomeR
        author = "Snowdrift.coop Team"
        description = "Deployments of the Snowdrift.coop site"
        lang = "en"
        time :: UTCTime
        (time:_) = map buildBootTime builds

    render <- getUrlRender

    entries :: [FeedEntry (Route App)] <- forM builds $ \build -> do
        let prettyDiff = mapM_ (\line -> toHtml line >> br) $ T.lines $ buildDiff build
            html = [hamlet|
                       <pre>
                            #{prettyDiff}
                   |]
            entry = FeedEntry (ProjectR "snowdrift") (buildBootTime build) (buildBase build) (html render)

        return entry

    newsFeed $ Feed title feed_url home_url author description lang time entries

