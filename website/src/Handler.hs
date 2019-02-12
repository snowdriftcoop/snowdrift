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

getWelcomeR :: Handler Html
getWelcomeR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutNew $ do
        setTitle "Crowdmatching for Public Goods"
        $(widgetFile "page/welcome")

getHowItWorksR :: Handler Html
getHowItWorksR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutNew $ do
        setTitle "How it Works"
        $(widgetFile "page/how-it-works")

getPrivacyR :: Handler Html
getPrivacyR = defaultLayoutNew $ do
    setTitle "Privacy Policy"
    $(widgetFile "page/privacy")

getTermsR :: Handler Html
getTermsR = $(widget "page/terms" "Terms of Service")

getProjectsR :: Handler Html
getProjectsR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutNew $ do
        setTitle "Projects"
        $(widgetFile "page/projects")

-- Redirect /p to /projects
getPR :: Handler Html
getPR = redirect ProjectsR

getTrademarksR :: Handler Html
getTrademarksR = $(widget "page/trademarks" "Trademarks")

getDonateR :: Handler Html
getDonateR = $(widget "page/donate" "Donate")

getAboutR :: Handler Html
getAboutR = defaultLayoutNew $ do
    setTitle "About"
    $(widgetFile "page/about")

getSponsorsR :: Handler Html
getSponsorsR = $(widget "page/sponsors" "Sponsors")

getJsLicensesR :: Handler Html
getJsLicensesR = $(widget "page/js-licenses" "JavaScript Licenses")

getMerchandiseR :: Handler Html
getMerchandiseR = $(widget "page/merchandise" "Merchandise")

getContactR :: Handler Html
getContactR = $(widget "page/contact" "Contact")

-- | Prevents breakage of external links to the old blog.
getSnowdriftLegacyBlogBaseR :: Handler Html
getSnowdriftLegacyBlogBaseR =
    redirectWith movedPermanently301 ("https://blog.snowdrift.coop/" :: Text)

getSnowdriftLegacyBlogR :: Text -> Handler Html
getSnowdriftLegacyBlogR slug =
    redirectWith movedPermanently301 $ "https://blog.snowdrift.coop/" <> slug

-- Redirect to new wiki
getSnowdriftWikiR :: Handler Html
getSnowdriftWikiR = redirect ("https://wiki.snowdrift.coop/" :: Text)

-- | Prevents breakage of external links to the old wiki
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

-- Redirect /u/3 to wiki.sn.../community/people/wolftune
getLegacyWolftuneR :: Handler Html
getLegacyWolftuneR =
    redirect ("https://wiki.snowdrift.coop/community/people/wolftune" :: Text)
