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

newtype ChargeCents =
    ChargeCents Int32
    deriving (Show, Num, Integral, Enum, Real, Ord, Eq)

-- | DonationUnits are truncated to usable cents for use in creating charges.
chargeCents :: Iso' DonationUnit ChargeCents
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
stripeFee :: ChargeCents -> ChargeCents
stripeFee = round . (+ 30) . (* 0.029) . fromIntegral

-- | A donation is sufficient for processing if the Stripe fee is < 10%.
-- https://tree.taiga.io/project/snowdrift/issue/457
--
-- This function is useful for testing, but we memoize its
-- production-required result below.
sufficientDonation :: DonationUnit -> Bool
sufficientDonation d =
    fee % (fee + view chargeCents d) < maximumFee
  where
    fee = stripeFee (d^.chargeCents)
    maximumFee = ((%) `on` ChargeCents) 1 10

-- | This is the minimum amount that satisfies 'sufficientDonation'. You can
-- find it for yourself by running:
-- >>> :{
-- >>> let [x] = take 1 . filter (\x -> all sufficientDonation [x..x+35])
-- >>>                  . map DonationUnit
-- >>>                  $ [10..]
-- >>> in (x, x == minimumDonation)
-- >>>:}
-- (DonationUnit 3700,True)
--
-- Note that rounding makes the function discontinuous, with a step every
-- 1/0.029 ~ 35 DonationUnits. There's a local optimum at ~3610, but we'll just
-- skip that one, cause that's weird.
--
-- By arithmetic instead of brute force, we have (in cents):
--
--     fee / (fee + donation) < 1/10
--     9 * fee < donation
--     9 * round(0.029 * donation + 30) < donation
--
-- Without rounding, we could continue:
--
--     270 / (1 - 9*0.029) ~<~ donation
--     365.4 ~<~ donation
--
-- With rounding, however, the fee stays fixed for a range of donation values.
-- In fact,
--
--     fee  = 41 | 363 <= donation <= 396
--     fee >= 42 | 397 <= donation
--
-- In the first range, that gives:
--
--     9 * 41 < donation  --> donation <= 370
--
-- In the second range,
--
--     9 * 42 < donation --> donation <= 378.
--
-- That's less than the beginning of that range, so all further values are ok.
--
-- Thus, the minimum donation is 370 cents, or 3700 DonationUnits.
--
minimumDonation :: DonationUnit
minimumDonation = DonationUnit 3700

makePayments :: IO ()
makePayments = do
    charges <- runPersist (patronBalances minimumDonation)
    -- send a bunch of charges to Stripe, and then deal with the return values
    mapM_ print charges
