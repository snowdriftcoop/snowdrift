{-# LANGUAGE QuasiQuotes #-}
module Handler.TH where

import Import
import Handler.Util

import Language.Haskell.TH

widget :: String -> Text -> Q Exp
widget name title =
    [|navbarLayout name (snowdriftTitle title >> $(widgetFile name))|]
