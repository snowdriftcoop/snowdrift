module Handler.Privacy where

import Import

getPrivacyR :: Handler RepHtml
getPrivacyR = defaultLayout $(widgetFile "priv")
