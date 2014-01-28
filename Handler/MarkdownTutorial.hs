module Handler.MarkdownTutorial where

import Import



getMarkdownTutorialR :: Handler Html
getMarkdownTutorialR = defaultLayout $ do
    setTitle "Markdown Tutorial | Snowdrift.coop"
    $(widgetFile "markdown")
