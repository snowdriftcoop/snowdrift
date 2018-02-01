{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Crowdmatch.Stripe
    ( StripeDummy (..)
    , stripe
    )
where

import Data.Aeson.Types
import Data.Time.Calendar
import Data.Time.Clock
import Web.Stripe hiding   (stripe)
import Web.Stripe.Balance  ( GetBalanceTransaction
                           , BalanceTransaction (..)
                           , Currency (..)
                           , TransactionId (..)
                           , TransactionType (..)
                           )
import Web.Stripe.Charge   ( CreateCharge
                           , Amount (..)
                           , Charge (..)
                           , ChargeId (..)
                           )
import Web.Stripe.Customer ( Customer (..)
                           , CreateCustomer
                           , UpdateCustomer
                           , DeleteCustomer
                           , CustomerId (..)
                           , MetaData (..)
                           , StripeDeleteResult (..)
                           , StripeList (..)
                           )

import qualified Data.Text as T
import qualified Web.Stripe as S (stripe)

-- | A typeclass for Stripe requests (such as 'CreateCustomer') which can be
-- answered with dummy data without any HTTP requests to Stripe itself. We're
-- allowing the dummy data generator access the request and do IO actions, but
-- these things can be removed to simplify things if they aren't needed (for
-- example if the dummy data is just hardcoded).
class StripeDummy a where
    -- | Provide a response to the Stripe request with some dummy data, without
    -- talking to Stripe itself.
    stripeDummy :: StripeRequest a -> IO (StripeReturn a)

-- | Fill a Stripe request, either by talking to the real Stripe or by
-- providing some dummy data for development or testing.
--
-- Currently the decision is made using the Stripe key: If the key is `test`
-- then dummy data is provided. Otherwise, the real Stripe API is contacted by
-- HTTP request and its response returned.
stripe :: (FromJSON (StripeReturn a), StripeDummy a)
       => StripeConfig
       -> StripeRequest a
       -> IO (Either StripeError (StripeReturn a))
stripe c s
    | secretKey c == StripeKey "test" = Right <$> stripeDummy s
    | otherwise                       = S.stripe c s

dummyCustomer :: Customer
dummyCustomer  = Customer
    T.empty
    (UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0))
    (CustomerId "dummy")
    False
    Nothing
    Nothing
    False
    (StripeList [] T.empty T.empty Nothing False)
    Nothing
    0
    (StripeList [] T.empty T.empty Nothing False)
    Nothing
    Nothing
    (MetaData [])

dummyDelete :: StripeDeleteResult
dummyDelete  = StripeDeleteResult True Nothing

dummyCharge :: Charge
dummyCharge  = Charge
                    (ChargeId T.empty)
                    T.empty
                    (UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0))
                    False
                    False
                    (Amount 0)
                    USD
                    False
                    Nothing
                    False
                    (StripeList [] T.empty T.empty Nothing False)
                    Nothing
                    Nothing
                    Nothing
                    0
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (MetaData [])
                    Nothing
                    Nothing
                    Nothing

dummyBalanceTransaction :: BalanceTransaction
dummyBalanceTransaction  = BalanceTransaction
                                (TransactionId T.empty)
                                T.empty
                                0
                                USD
                                0
                                ChargeTxn
                                (UTCTime (fromGregorian 2018 1 1) 
                                         (secondsToDiffTime 0))
                                (UTCTime (fromGregorian 2018 1 1)
                                         (secondsToDiffTime 0))
                                T.empty
                                0
                                []
                                (Id $ ChargeId T.empty)
                                Nothing
                                

instance StripeDummy CreateCustomer where
    stripeDummy _ = return dummyCustomer

instance StripeDummy UpdateCustomer where
    stripeDummy _ = return dummyCustomer

instance StripeDummy DeleteCustomer where
    stripeDummy _ = return dummyDelete

instance StripeDummy CreateCharge where
    stripeDummy _ = return dummyCharge

instance StripeDummy GetBalanceTransaction where
    stripeDummy _ = return dummyBalanceTransaction
