module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayoutV2 $ do
    setTitle "About"
    $(widgetFile "page/about")
