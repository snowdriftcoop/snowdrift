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
-- https://hackage.haskell.org/package/stripe-haskell-2.6.2/docs/Web-Stripe.html
import Web.Stripe (StripeKey(..), StripeConfig(..))
import System.Environment (lookupEnv)
import Control.Error (runScript, (!?), scriptIO)

import Crowdmatch
import RunPersist

-- Needs STRIPE_SECRET_KEY environment variable
main :: IO ()
main = runScript $ do
    conf <- fmap -- ExceptT String IO
        -- Stripe config has these two fields:
            -- secretKey :: StripeKey
            -- stripeEndpoint :: Maybe Endpoint
            -- Not sure where the endpoint comes from, 'endpoint' doesn't appear anywhere else in the code base
            -- Maybe webhook endpoint
        (StripeConfig . StripeKey . pack) -- Pack converts from bytestring to char list (maybe)
        (lookupEnv "STRIPE_SECRET_KEY" !? "Missing STRIPE_SECRET_KEY in env")
    -- NB! The string passed to runPersistKeter must match the APPNAME used in
    -- keter.sh to deploy the app. Must fix. (Duplicate comment from
    -- CrowdmatchMain.)
    scriptIO $
        runPersistKeter "SnowdriftReboot" $ -- runPersistKeter just chooses which db we use
        makePayments $ -- from Crowdmatch.hs
        stripeProduction conf
