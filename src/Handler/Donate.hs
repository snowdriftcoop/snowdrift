module Handler.Donate where

import Import

import Handler.Utils

getDonateR :: Handler Html
getDonateR =
    defaultLayout $ do
        snowdriftTitle "Donate"
        $(widgetFile "donatepage")
