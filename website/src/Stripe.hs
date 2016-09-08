{-# LANGUAGE FlexibleContexts #-}

-- | Make Stripe available for Snowdrift Handlers. It provides 'snowstripe',
-- which is just 'Web.Stripe.stripe' partially applied in the Handler
-- monad.
module Stripe
  ( module Web.Stripe
  , snowstripe
  ) where

import Import

import Web.Stripe
import Web.Stripe.Error

snowstripe
    :: FromJSON (StripeReturn a)
    => StripeRequest a -> Handler (Either StripeError (StripeReturn a))
snowstripe req = do
    snowconf <- StripeConfig . appStripeSecretKey . appSettings <$> getYesod
    liftIO (stripe snowconf req)
