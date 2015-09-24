module Handler.Trademarks where

import Import
import Widgets.Doc

getTrademarksR :: Handler Html
getTrademarksR = defaultLayout $ do
    snowdriftTitle "Trademarks"
    renderDoc "Trademarks"
