module Handler.Privacy where

import Import

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $(widgetFile "priv")
