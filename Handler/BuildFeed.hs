module Handler.BuildFeed where

import Import

import Yesod.Feed

import qualified Data.Text as T

import Text.Blaze.Html5 (br)

getBuildFeedR :: Handler TypedContent
getBuildFeedR = do
    builds :: [Build] <- fmap (map entityVal) $ runDB $ selectList [] [Desc BuildBootTime]

    let title = T.pack $ "Snowdrift Deployments"
        feed_url = BuildFeedR
        home_url = HomeR
        author = "Snowdrift Team"
        description = "Deployments of the Snowdrift site"
        lang = "en"
        time :: UTCTime
        (time:_) = map buildBootTime builds

    render <- getUrlRender

    entries :: [FeedEntry (Route App)] <- forM builds $ \ build -> do
        let prettyDiff = mapM_ (\ line -> toHtml line >> br) $ T.lines $ buildDiff build
            html = [hamlet|
                       <pre>
                            #{prettyDiff}
                   |]
            entry = FeedEntry (ProjectR "snowdrift") (buildBootTime build) (buildBase build) (html render)

        return entry

    newsFeed $ Feed title feed_url home_url author description lang time entries
