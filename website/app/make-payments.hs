{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | make-payments
--
-- Inspects outstanding balances and sends appropriate charge commands to
-- Stripe.
--
module Main where

import Import.NoFoundation

import Control.Lens
import Data.Function (on)
import Data.Ratio
import RunPersist

import Model.Skeleton (patronBalances)

main :: IO ()
main = makePayments

newtype ChargeCent =
    ChargeCent Int32
    deriving (Show, Num, Integral, Enum, Real, Ord, Eq)

-- | DonationUnits are rounded to usable cents for use in creating charges.
chargeCents :: Iso' DonationUnit ChargeCent
chargeCents = iso toCents fromCents
  where
    fromCents = fromIntegral . (* 10)
    toCents = fromIntegral . (`div` 10)

-- | Calculate Stripe's fee: 2.9% + 30¢
--
-- https://stripe.com/us/pricing
--
-- Stripe uses financial rounding, aka the rounding everyone outside the US
-- learns (apparently). This is the rounding implemented in Prelude, as
-- well. Hooray!
--
-- If we ever have integration testing, we should confirm the following
-- holds true:
--
--      $5.00 charge -> 44.5¢ fee -> Stripe rounded to 44¢
--     $15.00 charge -> 73.5¢ fee -> Stripe rounded to 74¢
--
-- I confirmed these facts when I wrote this function, but tests ftw.
stripeFee :: DonationUnit -> ChargeCent
stripeFee =
    ChargeCent . round . (+ 30) . (* 0.029) . fromIntegral . view chargeCents

-- | A charge is sufficient for processing if the Stripe fee is < 10%.
-- https://tree.taiga.io/project/snowdrift/issue/457
--
-- This function is useful for testing, but we memoize its
-- production-required result below.
sufficientCharge :: DonationUnit -> Bool
sufficientCharge x =
    stripeFee x % view chargeCents x < maximumFee
  where
    maximumFee = ((%) `on` ChargeCent) 1 10

-- | This is the minimum payment that satisfies 'sufficientCharge'. You can
-- find it for yourself by running:
-- >>> :{
-- >>> [minimumPayment] ==
-- >>>     (take 1 . filter sufficientCharge . map DonationUnit $ [10..])
-- >>> :}
-- True
minimumPayment :: DonationUnit
minimumPayment = DonationUnit 4210

makePayments :: IO ()
makePayments = do
    charges <- runPersist $ patronBalances minimumPayment
    -- send a bunch of charges to Stripe, and then deal with the return values
    mapM_ print charges
