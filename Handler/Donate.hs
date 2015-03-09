{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Donate where

import Import

getDonateR :: Handler Html
getDonateR =
    defaultLayout $ do
        snowdriftTitle "Donate"
        $(widgetFile "donatepage")
