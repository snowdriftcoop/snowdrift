module Handler.Util (snowdriftTitle, snowdriftDashTitle, snowstripe) where

import Import.NoFoundation

import Data.Text.Titlecase
import Web.Stripe
import Web.Stripe.Error

import AppDataTypes

snowdriftTitle :: MonadWidget m => Text -> m ()
snowdriftTitle t = setTitle $
    toHtml (titlecase t) `mappend` toHtml (" | Snowdrift.coop" :: Text)

snowdriftDashTitle :: MonadWidget m => Text -> Text -> m ()
snowdriftDashTitle x y = snowdriftTitle $ x `mappend` " — " `mappend` y

snowstripe
    :: (Typeable (StripeReturn a), FromJSON (StripeReturn a))
    => StripeRequest a -> Handler (Either StripeError (StripeReturn a))
snowstripe req = do
    (func, conf) <-
        (appStripe &&& StripeConfig . appStripeSecretKey . appSettings) <$> getYesod
    liftIO (func conf req)
