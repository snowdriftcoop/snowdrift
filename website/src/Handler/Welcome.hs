module Handler.Welcome where

import Import

getWelcomeR :: Handler Html
getWelcomeR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutV2 $ do
        setTitle "Crowdmatching for Public Goods"
        $(widgetFile "page/welcome")

