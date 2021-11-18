module Handler.Privacy where

import Import

getPrivacyR :: Handler Html
getPrivacyR = defaultLayoutV2 $ do
    setTitle "Privacy Policy"
    $(widgetFile "page/privacy")
