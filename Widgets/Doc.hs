module Widgets.Doc where

import Import
import Model.Markdown
import Widgets.Sidebar

renderDoc :: Text -> Widget
renderDoc name = do
    page <- handlerToWidget $ runDB $ get404 . docCurrentVersion . entityVal =<< getBy404 (UniqueDocName name)

    $(widgetFile "doc")

    
    
