-- | Common handler functions.
module Handler where

import Import hiding ((.=))

import Data.FileEmbed (embedFile)
import qualified Data.HashMap.Strict as H

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

-- | Prevents breakage of external links to the old blog.
getSnowdriftLegacyBlogR :: Text -> Handler Html
getSnowdriftLegacyBlogR slug =
    redirectWith movedPermanently301 $ "https://wiki.snowdrift.coop/blog/" <> slug

-- Redirect to new wiki
getSnowdriftWikiR :: Handler Html
getSnowdriftWikiR = redirect ("https://wiki.snowdrift.coop/" :: Text)

-- | Prevents breakage of external links to the old wiki. See
-- https://tree.taiga.io/project/snowdrift/us/359 and
-- https://tree.taiga.io/project/snowdrift/task/446
getSnowdriftWikiSearchR :: Text -> Handler Html
getSnowdriftWikiSearchR slug =
    redirect $
        mappend "https://wiki.snowdrift.coop/" $
            fromMaybe ("_search?patterns=" <> slug) (H.lookup slug routes)
  where
    (.=) = (,)
    -- The choice of datastructure is arbitrary here.
    routes :: HashMap Text Text
    routes = H.fromList
        [ "en" .= ""
        , "about" .= "about"
        , "conduct" .= "community/conduct"
        , "economics" .= "about/economics"
        , "existingmechanisms" .= "about/existing-mechanisms"
        , "formats-repositories" .= "market-research/flo-repos"
        , "free-libre-open" .= "about/free-libre-open"
        , "honor" .= "community/honor"
        , "honor-projects" .= "project-reqs/honor-projects"
        , "honor-users" .= "community/honor-users"
        , "how-to-help" .= "community/how-to-help"
        , "intro" .= "about/intro"
        , "irc" .= "community/irc"
        , "legal" .= "legal"
        , "licenses" .= "about/licenses"
        , "limits" .= "about/limits"
        , "mechanism" .= "about/mechanism"
        , "mission" .= "about/mission"
        , "next" .= "planning/next"
        , "othercrowdfunding" .= "market-research/other-crowdfunding"
        , "payment-services" .= "market-research/payment-services"
        , "presentations" .= "about/presentations"
        , "psychology" .= "about/psychology"
        , "snowdrift" .= "about/snowdrift-dilemma"
        , "status-quo-floss" .= "market-research/history/software"
        , "threshold-systems" .= "about/threshold-systems"
        , "whyfree" .= "about/why-flo"
        ]

-- Yesod is being fussy, and won't let me have two URLs go to the same handler
getSnowdriftWikiEnSearchR :: Text -> Handler Html
getSnowdriftWikiEnSearchR = getSnowdriftWikiSearchR

-- Redirect /who to community/team
getLegacyWhoR :: Handler Html
getLegacyWhoR = redirect ("https://wiki.snowdrift.coop/community/team" :: Text)

-- Redirect /u/3 to community/team
getLegacyWolftuneR :: Handler Html
getLegacyWolftuneR = redirect ("https://wiki.snowdrift.coop/community/team/wolftune" :: Text)
