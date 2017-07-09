module Alerts
        ( alertDanger
        , alertInfo
        , alertSuccess
        , alertWarning
        ) where

import Yesod
import Data.Text (Text)

addAlert :: MonadHandler m => Text -> Html -> m ()
addAlert = addMessage

alertDanger, alertInfo, alertSuccess, alertWarning
    :: MonadHandler m => Html -> m ()
alertDanger  = addAlert "danger"
alertInfo    = addAlert "info"
alertSuccess = addAlert "success"
alertWarning = addAlert "warning"
