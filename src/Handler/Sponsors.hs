module Handler.Sponsors where

import Import

getSponsorsR :: Handler Html
getSponsorsR =
    defaultLayout $ do
        snowdriftTitle "Sponsors"
        $(widgetFile "sponsors")
