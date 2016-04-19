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
-- getHomeR = getDashboardR `catch` bleh
--   where
--     bleh :: HandlerContents -> Handler Html
--     bleh = const getWelcomeR
getHomeR :: Handler Html
getHomeR = do
    u <- maybeAuth
    maybe getWelcomeR
          (const $(widget "page/dashboard" "Dashboard"))
          u

getWelcomeR :: Handler Html
getWelcomeR = $(widget "page/welcome" "Snowdrift.coop — Free the Commons")

getDashboardR :: Handler Html
getDashboardR = do
    _ <- requireAuth
    $(widget "page/dashboard" "Dashboard")

getHowItWorksR, getPrivacyR, getTermsR, getResetPassphraseR, getCreateAccountR, getSearchR, getProjectsR :: Handler Html
getHowItWorksR = defaultLayout [whamlet|HOWITWORKS|]
getPrivacyR = defaultLayout [whamlet|PRIVACY|]
getTermsR = defaultLayout [whamlet|TERMS|]
getResetPassphraseR = defaultLayout [whamlet|RESETPASSPHRASE|]
getCreateAccountR = defaultLayout [whamlet|CREATEACCOUNT|]
getSearchR = defaultLayout [whamlet|SEARCH|]
getProjectsR = defaultLayout [whamlet|PROJECTS|]
getTrademarksR :: Handler Html
getTrademarksR = defaultLayout [whamlet|Trademarks|]
getDonateR :: Handler Html
getDonateR = defaultLayout [whamlet|Donate|]
getAboutR :: Handler Html
getAboutR = defaultLayout [whamlet|About|]
getSponsorsR :: Handler Html
getSponsorsR = defaultLayout [whamlet|Sponsors|]
getJsLicensesR :: Handler Html
getJsLicensesR = defaultLayout [whamlet|JsLicenses|]
