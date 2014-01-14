module Handler.Tos where

import Import
import Widgets.Doc

getTosR :: Handler Html
getTosR = defaultLayout $ renderDoc "Terms of Use"
