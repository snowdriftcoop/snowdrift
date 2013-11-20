module Handler.Tos where

import Import

getTosR :: Handler Html
getTosR = defaultLayout $(widgetFile "tos")
