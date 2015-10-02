module Handler.ToU where

import Import
import Handler.Utils
import Widgets.Doc

getToUR :: Handler Html
getToUR = defaultLayout $ do
    snowdriftTitle "Terms of Use"
    renderDoc "Terms of Use"
