{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
    defaultLayoutNew $ do
        setTitle
            "Snowdrift.coop | Clearing the path to a Free/Libre/Open world"
        $(widgetFile "homepage")
