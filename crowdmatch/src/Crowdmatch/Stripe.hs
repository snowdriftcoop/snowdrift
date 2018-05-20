{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This is a wrapper for all Stripe actions used by Snowdrift. It's not
-- specific just to crowdmatch and may involve actions the web app uses and
-- crowdmatch does not, so technically we could make it a separate library. But
-- it feels like too much for a tiny single internal module, so for now it's
-- here.
module Crowdmatch.Stripe
    ( StripeActions (..)
    , chargeCents
    , stripeProduction
    , StripeDevState (..)
    )
where

import Control.Arrow (right)
import Control.Lens (Iso', iso, view)
import Web.Stripe (stripe, (-&-), StripeConfig, StripeError)
import Web.Stripe.Balance
import Web.Stripe.Charge

import qualified Web.Stripe.Customer as SC

import Crowdmatch.ModelDataTypes

data StripeActions = StripeActions
    { createCustomer
        :: TokenId -> IO (Either StripeError Customer)
    , updateCustomer
        :: TokenId -> CustomerId -> IO (Either StripeError Customer)
    , deleteCustomer
        :: CustomerId -> IO (Either StripeError ())
    , chargeCustomer
        :: CustomerId -> Cents -> IO (Either StripeError Charge)
    , balanceTransaction
        :: TransactionId -> IO (Either StripeError BalanceTransaction)
    }

-- | Stripe measures charges in cents. Handy!
chargeCents :: Iso' Cents Amount
chargeCents = iso toAmount fromAmount
  where
    toAmount (Cents i) = Amount (fromIntegral i)
    fromAmount (Amount i) = Cents (fromIntegral i)

stripeProduction :: StripeConfig -> StripeActions
stripeProduction c = StripeActions
    { createCustomer =
        \ cardToken -> stripe c $ SC.createCustomer -&- cardToken
    , updateCustomer =
        \ cardToken cust -> stripe c $ SC.updateCustomer cust -&- cardToken
    , deleteCustomer =
        \ cust -> right (const ()) <$> stripe c (SC.deleteCustomer cust)
    , chargeCustomer =
        \ cust cents ->
              stripe c
            . (-&- cust)
            -- Supported upstream as of 2016-10-06, but not in our resolver yet
            -- . (-&- ExpandParams ["balance_transaction"])
            . flip createCharge USD
            . view chargeCents
            $ cents
    , balanceTransaction =
        \ transId -> stripe c $ getBalanceTransaction transId
    }

newtype StripeDevState = StripeDevState { lastCharge :: Maybe Cents }
    deriving (Eq, Show)
