module Handler.ToU where

import Import
import Widgets.Doc

getToUR :: Handler Html
getToUR = defaultLayout $ do
    setTitle "Terms of Use | Snowdrift.coop"
    renderDoc "Terms of Use"
