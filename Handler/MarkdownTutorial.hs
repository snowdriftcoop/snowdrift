module Handler.MarkdownTutorial where

import Import



getMarkdownTutorialR :: Handler Html
getMarkdownTutorialR = defaultLayout $ do
    snowdriftTitle "Markdown Tutorial"
    $(widgetFile "markdown")
