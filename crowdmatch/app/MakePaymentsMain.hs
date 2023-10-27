{-# LANGUAGE CPP #-}
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
import Data.List (partition)
-- import Data.Text (read)
-- Note: There have been some api changes since the version we're using
-- https://hackage.haskell.org/package/stripe-haskell-2.2.3/docs/Web-Stripe.html
-- https://hackage.haskell.org/package/stripe-core-2.3.0/docs/Web-Stripe-Client.html
import Web.Stripe (StripeKey(..), StripeConfig(..))
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Control.Error (runScript, (!?), scriptIO)

import Crowdmatch
import RunPersist

usage :: IO a -- exits
usage = do
    putStrLn "usage examples:"
    putStrLn "  make-payments --all"
    putStrLn "  make-payments --team-only"
    exitWith (ExitFailure 2)

-- Needs STRIPE_SECRET_KEY environment variable
runPayments :: Bool -> IO ()
runPayments teamOnly = runScript $ do
    conf <- fmap -- ExceptT String IO
        -- Stripe config just wraps one field, secretKey :: StripeKey
        (mkConfig . StripeKey . pack) -- Pack converts from bytestring to char list (maybe)
        (lookupEnv "STRIPE_SECRET_KEY" !? "Missing STRIPE_SECRET_KEY in env")
    -- NB! The string passed to runPersistKeter must match the APPNAME used in
    -- keter.sh to deploy the app. Must fix. (Duplicate comment from
    -- CrowdmatchMain.)
    scriptIO $
        runPersistKeter "SnowdriftReboot" $ -- runPersistKeter just chooses which db we use
        -- from Crowdmatch.hs
        makePayments (stripeProduction conf) teamOnly
  where
-- Not sure when this change happened, but it's greater than 2.2.3 and no later than 2.6.
#if MIN_VERSION_stripe_haskell(2,3,0)
    mkConfig k = StripeConfig k Nothing
#else
    mkConfig = StripeConfig
#endif


main :: IO ()
main = do
    args <- getArgs
    (options, _) <- return (partition startsWithDashes args)
    case options of
        "--all" : _ -> runPayments False
        "--team-only" : _ -> runPayments True
        _ -> usage

    where
        startsWithDashes letters = (take 2 letters) == "--"
