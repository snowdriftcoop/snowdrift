module Handler.Faq where

import Import

-- import Widgets.Sidebar

getFaqR :: Handler Html
getFaqR = do
    setMessage "We don't have a list of frequently asked questions yet - hopefully the about page will answer yours."
    redirect $ WikiR "snowdrift" "about"

