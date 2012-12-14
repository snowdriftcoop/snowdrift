module Handler.Tos where

import Import

getTosR :: Handler RepHtml
getTosR = defaultLayout $(widgetFile "tos")
