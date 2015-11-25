module Handler.Simple where

import Import

import Handler.Utils

getHomeR :: Handler Html
getHomeR =
    defaultLayoutNew "homepage" $ do
        snowdriftTitle "Free the Commons"
        $(widgetFile "homepage")

getIntroR :: Handler Html
getIntroR =
    defaultLayoutNew "intro" $ do
        snowdriftTitle "Some Title Here"
        $(widgetFile "intro")
