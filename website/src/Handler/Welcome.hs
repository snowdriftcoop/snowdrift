module Handler.Welcome where

import Import

import Handler.TH

getWelcomeR :: Handler Html
getWelcomeR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutV2 $ do
        setTitle "Crowdmatching for Public Goods"
        $(widgetFile "page/welcome")

