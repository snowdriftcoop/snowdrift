module Handler.Privacy where

import Import

import Handler.Utils
import Widgets.Doc

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
    snowdriftTitle "Privacy Policy"
    renderDoc "Privacy Policy"
