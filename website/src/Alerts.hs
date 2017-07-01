module Alerts
        ( alertDanger
        , alertInfo
        , alertSuccess
        , alertWarning
        ) where

import Prelude

import Yesod
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL

addAlert :: MonadHandler m => Text -> Html -> m ()
addAlert = addMessage

alertDanger, alertInfo, alertSuccess, alertWarning
    :: MonadHandler m => Html -> m ()
alertDanger  = addAlert "danger"
alertInfo    = addAlert "info"
alertSuccess = addAlert "success"
alertWarning = addAlert "warning"
