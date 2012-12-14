module Handler.MarkdownTutorial where

import Import

import Widgets.Sidebar

getMarkdownTutorialR :: Handler RepHtml
getMarkdownTutorialR = defaultLayout $(widgetFile "markdown")
