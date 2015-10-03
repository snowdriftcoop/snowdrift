module Handler.Sponsors where

import Import

import Handler.Utils

getSponsorsR :: Handler Html
getSponsorsR =
    defaultLayout $ do
        snowdriftTitle "Sponsors"
        $(widgetFile "sponsors")
