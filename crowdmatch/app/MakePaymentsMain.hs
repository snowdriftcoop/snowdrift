{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | make-payments
--
-- Inspects outstanding balances and sends appropriate charge commands to
-- Stripe.
--
module Main (main) where

import Data.ByteString.Char8 (pack)
import Web.Stripe (StripeKey(..), StripeConfig(..))
import System.Environment (lookupEnv)
import Control.Error (runScript, (!?), scriptIO)

import Crowdmatch
import RunPersist

main :: IO ()
main = runScript $ do
    conf <- fmap -- ExceptT String IO
        (StripeConfig . StripeKey . pack)
        (lookupEnv "STRIPE_SECRET_KEY" !? "Missing STRIPE_SECRET_KEY in env")
    -- NB! The string passed to runPersistKeter must match the APPNAME used in
    -- keter.sh to deploy the app. Must fix. (Duplicate comment from
    -- CrowdmatchMain.)
    scriptIO (runPersistKeter "SnowdriftReboot" (makePayments (runStripe conf)))
