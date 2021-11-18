module Handler.Privacy where

import Import

import Handler.TH

getPrivacyR :: Handler Html
getPrivacyR = defaultLayoutV2 $ do
    setTitle "Privacy Policy"
    $(widgetFile "page/privacy")
