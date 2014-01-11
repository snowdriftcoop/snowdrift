module Handler.Faq where

import Import

getFaqR :: Handler Html
getFaqR = do
    addAlert "warning" "We don't have a list of frequently asked questions yet - hopefully the about page will answer yours." 
    redirect $ WikiR "snowdrift" "about"

