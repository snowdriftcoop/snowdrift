{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Widgets.Sidebar

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $(widgetFile "homepage")

