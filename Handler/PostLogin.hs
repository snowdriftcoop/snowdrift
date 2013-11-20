module Handler.PostLogin where

import Import

getPostLoginR :: Handler Html
getPostLoginR = do
    app <- getYesod
    redirectUltDest $ loginDest app
