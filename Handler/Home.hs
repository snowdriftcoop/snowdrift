{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        snowdriftTitle "Clearing the path to a Free/Libre/Open world"
        $(widgetFile "homepage")
