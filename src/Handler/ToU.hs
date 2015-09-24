module Handler.ToU where

import Import
import Widgets.Doc

getToUR :: Handler Html
getToUR = defaultLayout $ do
    snowdriftTitle "Terms of Use"
    renderDoc "Terms of Use"
