-- | Common handler functions.
module Handler where

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

getWelcomeR, getDashboardR, getHowItWorksR, getPrivacyR, getTermsR, getResetPassphraseR, getCreateAccountR, getSearchR, getProjectsR :: Handler Html
getWelcomeR = defaultLayout [whamlet|WELCOME|]
getDashboardR = defaultLayout [whamlet|DASHBOARD|]
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
getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "homepage")
