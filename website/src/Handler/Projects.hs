module Handler.Projects where

import Import

import Handler.TH

getProjectsR :: Handler Html
getProjectsR = do
    loggedIn <- isJust <$> maybeAuth
    defaultLayoutV2 $ do
        setTitle "Projects"
        $(widgetFile "page/projects")
