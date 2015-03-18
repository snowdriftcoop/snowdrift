module Handler.Widget where

import Import

import Text.Hamlet (hamletFile)

import Model.Project
import Model.Currency


widgetLayout :: WidgetT App IO () -> HandlerT App IO Html
widgetLayout widget = do
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_min_css
        widget
    withUrlRenderer $(hamletFile "templates/widget-wrapper.hamlet")


getWidgetR :: Text -> Handler Html
getWidgetR project_handle = do
    (project, pledges) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- fetchProjectSharesDB project_id
        return (project, pledges)

    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = fromIntegral $ sum pledges
        project_value = share_value $* shares

    widgetLayout $(widgetFile "widget")

