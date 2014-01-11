module Handler.MarkdownTutorial where

import Import



getMarkdownTutorialR :: Handler Html
getMarkdownTutorialR = defaultLayout $(widgetFile "markdown")
