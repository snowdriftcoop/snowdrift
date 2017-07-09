{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | make-payments
--
-- Inspects outstanding balances and sends appropriate charge commands to
-- Stripe.
--
module Main (main) where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Web.Stripe (StripeKey(..), StripeConfig(..))
import System.Environment (lookupEnv)

import Crowdmatch
import RunPersist

main :: IO ()
main = do
    conf <- fmap
        (StripeConfig . StripeKey . pack . fromJust)
        (lookupEnv "STRIPE_SECRET_KEY")
    -- NB! The string passed to runPersistKeter must match the APPNAME used in
    -- keter.sh to deploy the app. Must fix. (Duplicate comment from
    -- CrowdmatchMain.)
    runPersistKeter "SnowdriftReboot" (makePayments (runStripe conf))
