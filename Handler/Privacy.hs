module Handler.Privacy where

import Import

import Widgets.Doc

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ renderDoc "Privacy Policy"
