-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getWelcomeR, getDashboardR, getHowItWorksR :: Handler Html
getWelcomeR = defaultLayout [whamlet|WELCOME|]
getDashboardR = defaultLayout [whamlet|DASHBOARD|]
getHowItWorksR = defaultLayout [whamlet|HOWITWORKS|]
