{-# LANGUAGE QuasiQuotes #-}
module Handler.TH where

import Import
import Handler.Util

import Language.Haskell.TH

-- | Generates things of type "Handler Html". Very useful for simple
-- handlers.
widget :: String -> Text -> Q Exp
widget name title =
    [|navbarLayout Cassius name (snowdriftTitle title >> $(widgetFile name))|]

widgetSass :: String -> Text -> Q Exp
widgetSass name title =
    [|navbarLayout Sass name (snowdriftTitle title >> $(widgetFile name))|]
