module Handler.HowItWorks where

import Import

import Handler.TH

getHowItWorksR :: Handler Html
getHowItWorksR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutV2 $ do
        setTitle "How it Works"
        $(widgetFile "page/how-it-works")
