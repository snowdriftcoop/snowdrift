{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Simple where

import Import

import Handler.Utils

getHomeR :: Handler Html
getHomeR =
    defaultLayoutNew $ do
        snowdriftTitle "Free the Commons"
        $(widgetFile "homepage")
