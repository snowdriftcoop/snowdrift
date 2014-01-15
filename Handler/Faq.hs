module Handler.Faq where

import Import

getFaqR :: Handler Html
getFaqR = do
    addAlert "warning" "We don't have a real FAQ at this time. Here's our general about page. See also the links at the bottom. Each page also has connected discussions." 
    redirect $ WikiR "snowdrift" "about"
