{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle
            "Snowdrift.coop | Free the Commons"
        $(widgetFile "homepage")
