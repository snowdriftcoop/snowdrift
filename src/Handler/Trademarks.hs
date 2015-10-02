module Handler.Trademarks where

import Import
import Handler.Utils
import Widgets.Doc

getTrademarksR :: Handler Html
getTrademarksR = defaultLayout $ do
    snowdriftTitle "Trademarks"
    renderDoc "Trademarks"
