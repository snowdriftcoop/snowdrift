-- This file generates https://snowdrift.coop/dev/build which is a feed showing updates to the live Snowdrift.coop site

module Handler.BuildFeed where

import Import

import Text.Blaze.Html5 (br)
import Yesod.Feed
import qualified Data.Text as T

getBuildFeedR :: Handler TypedContent
getBuildFeedR = do
    builds :: [Build] <- fmap (map entityVal) $ runDB $ selectList [] [Desc BuildBootTime]

    render <- getUrlRender
    -- Have to render internal links, since feed items are external and we
    -- have to use newsFeedText.
    let title = T.pack "Snowdrift.coop Deployments"
        feed_url = render BuildFeedR
        home_url = render HomeR
        author = "Snowdrift.coop Team"
        description = "Deployments of the Snowdrift.coop site"
        lang = "en"
        time :: UTCTime
        (time:_) = map buildBootTime builds


    entries :: [FeedEntry Text] <- forM builds $ \build -> do
        let prettyDiff = mapM_ (\line -> toHtml line >> br) $ T.lines $ buildDiff build
            html = [hamlet|
                       <pre>
                            #{prettyDiff}
                   |]
            entry = FeedEntry (gitlab <> buildBase build)
                              (buildBootTime build)
                              (buildBase build)
                              (html render)
                              Nothing
        return entry

    newsFeedText $ Feed title feed_url home_url author description lang time Nothing entries
  where gitlab = "https://git.snowdrift.coop/sd/snowdrift/commit/"
