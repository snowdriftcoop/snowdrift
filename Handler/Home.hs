{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Widgets.Sidebar

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "homepage")

