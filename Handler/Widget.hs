module Handler.Widget where

import Import

import Text.Hamlet (hamletFile)

import Model.Project
import Model.Currency


widgetLayout :: GWidget App App () -> GHandler App App RepHtml
widgetLayout widget = do
    pc <- widgetToPageContent $ do
        $(widgetFile "normalize")
        addStylesheet $ StaticR css_bootstrap_css
        widget
    hamletToRepHtml $(hamletFile "templates/widget-wrapper.hamlet")


getWidgetR :: ProjectId -> Handler RepHtml
getWidgetR project_id = do
    (project, pledges) <- runDB $ (,)
        <$> get404 project_id
        <*> getProjectShares project_id

    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = fromIntegral $ sum pledges
        project_value = share_value $* shares
        
    widgetLayout $(widgetFile "widget")
    
