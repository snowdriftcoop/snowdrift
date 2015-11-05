module Handler.MarkdownTutorial where

import Import

import Handler.Utils

getMarkdownTutorialR :: Handler Html
getMarkdownTutorialR = defaultLayout $ do
    snowdriftTitle "Markdown Tutorial"
    $(widgetFile "markdown")
