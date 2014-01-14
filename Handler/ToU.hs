module Handler.ToU where

import Import
import Widgets.Doc

getToUR :: Handler Html
getToUR = defaultLayout $ renderDoc "Terms of Use"
