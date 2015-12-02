{-# LANGUAGE QuasiQuotes #-}
module Handler.TH where

import Import
import Handler.Utils

import Language.Haskell.TH

-- | Generates things of type "Handler Html". Very useful for simple
-- handlers.
simpleHandler :: String -> Text -> Q Exp
simpleHandler name title =
    [|defaultLayoutNew name (snowdriftTitle title >> $(widgetFile name))|]
