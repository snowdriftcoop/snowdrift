module Handler.Util (snowdriftTitle, snowdriftDashTitle) where

import Import

import Data.Text.Titlecase

snowdriftTitle :: MonadWidget m => Text -> m ()
snowdriftTitle t = setTitle $
    toHtml (titlecase t) `mappend` toHtml (" | Snowdrift.coop" :: Text)

snowdriftDashTitle :: MonadWidget m => Text -> Text -> m ()
snowdriftDashTitle x y = snowdriftTitle $ x `mappend` " — " `mappend` y
