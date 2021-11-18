module Handler.Terms where

import Import

getTermsR :: Handler Html
getTermsR = do
    defaultLayoutV2 $ do
        setTitle "Terms of Service"
        $(widgetFile "page/terms")
