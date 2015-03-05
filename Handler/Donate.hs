{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Donate where

import Import

getDonateR :: Handler Html
getDonateR =
    defaultLayout $ do
        setTitle "Donate to Snowdrift.coop"
        $(widgetFile "donatepage")
