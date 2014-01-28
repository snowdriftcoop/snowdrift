module Handler.Privacy where

import Import

import Widgets.Doc

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
    setTitle "Privacy Policy | Snowdrift.coop"
    renderDoc "Privacy Policy"
