module Handler.PostLogin where

import Import

getPostLoginR :: Handler RepHtml
getPostLoginR = do
    app <- getYesod
    redirectUltDest $ loginDest app
