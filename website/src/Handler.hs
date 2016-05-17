-- | Common handler functions.
module Handler where

import Import

import Data.FileEmbed (embedFile)

import Handler.TH

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

-- | Homepage is an introduction to the site for non-logged-in viewers, and
-- the dashboard for logged-in viewers.
getHomeR :: Handler Html
getHomeR = maybeAuth >>=
    maybe getWelcomeR
          (const $(widget "page/dashboard" "Dashboard"))

getWelcomeR :: Handler Html
getWelcomeR = $(widget "page/welcome" "Snowdrift.coop — Free the Commons")

getDashboardR :: Handler Html
getDashboardR = do
    _ <- requireAuth
    $(widget "page/dashboard" "Dashboard")

getHowItWorksR :: Handler Html
getHowItWorksR = $(widget "page/how-it-works" "getHowItWorksR")

getPrivacyR :: Handler Html
getPrivacyR = $(widget "page/privacy" "getPrivacyR")

getTermsR :: Handler Html
getTermsR = $(widget "page/terms" "getTermsR")

getSearchR :: Handler Html
getSearchR = do
    q <- lookupGetParam "q"
    $(widget "page/search" "Search")

getProjectsR :: Handler Html
getProjectsR = $(widget "page/projects" "getProjectsR")

getTrademarksR :: Handler Html
getTrademarksR = $(widget "page/trademarks" "getTrademarksR")

getDonateR :: Handler Html
getDonateR = $(widget "page/donate" "getDonateR")

getAboutR :: Handler Html
getAboutR = $(widget "page/about" "getAboutR")

getSponsorsR :: Handler Html
getSponsorsR = $(widget "page/sponsors" "getSponsorsR")

getJsLicensesR :: Handler Html
getJsLicensesR = $(widget "page/js-licenses" "getJsLicensesR")

getMerchandiseR :: Handler Html
getMerchandiseR = $(widget "page/merchandise" "getMerchandiseR")

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = $(widget "page/snowdrift-project" "Snowdrift.coop Project")

-- | Prevents breakage of external links to the old wiki. See
-- https://tree.taiga.io/project/snowdrift/us/359
getSnowdriftWikiSearchR :: Text -> Handler Html
getSnowdriftWikiSearchR slug =
    redirect $ "https://wiki.snowdrift.coop/_search?patterns=" <> slug
