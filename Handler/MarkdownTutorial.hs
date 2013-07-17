module Handler.MarkdownTutorial where

import Import

import Widgets.Sidebar

getMarkdownTutorialR :: Handler Html
getMarkdownTutorialR = defaultLayout $(widgetFile "markdown")
