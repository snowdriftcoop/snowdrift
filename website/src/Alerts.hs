module Alerts
        ( alertDanger
        , alertInfo
        , alertSuccess
        , alertWarning
        , getAlert
        ) where

import Prelude

import Control.Monad (liftM)
import Yesod
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL

alertKey :: Text
alertKey = "_MSG_ALERT"

addAlert :: MonadHandler m => Text -> Text -> m ()
addAlert level msg = do
    render <- getUrlRenderParams
    prev   <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $
        [hamlet|
        <div .alert .alert-#{level}>
          #{msg}
        |] render

alertDanger, alertInfo, alertSuccess, alertWarning :: MonadHandler m => Text -> m ()
alertDanger  = addAlert "danger"
alertInfo    = addAlert "info"
alertSuccess = addAlert "success"
alertWarning = addAlert "warning"

getAlert :: MonadHandler m => m (Maybe Html)
getAlert = do
    mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession alertKey
    deleteSession alertKey
    return mmsg
