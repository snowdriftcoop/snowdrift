module Handler.About where

import Import

import Handler.TH

getAboutR :: Handler Html
getAboutR = defaultLayoutV2 $ do
    setTitle "About"
    $(widgetFile "page/about")
