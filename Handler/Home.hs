{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Widgets.Sidebar

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "Snowdrift.coop | clearing the path to a Free/Libre/Open world"
        $(widgetFile "homepage")
