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
import Web.Stripe hiding (stripe)
import Web.Stripe.Balance (GetBalanceTransaction)
import Web.Stripe.Charge (CreateCharge)
import Web.Stripe.Customer ( Customer (..)
                           , CreateCustomer
                           , UpdateCustomer
                           , DeleteCustomer
                           , CustomerId (..)
                           , MetaData (..)
                           , StripeList (..))

import qualified Data.Text as T
import qualified Web.Stripe as S (stripe)

-- newtype StripeState = StripeState { lastCharge :: Maybe Cents } deriving (Eq, Show)

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
dummyCustomer = Customer
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

instance StripeDummy CreateCustomer where
    stripeDummy _ = return $ dummyCustomer

instance StripeDummy UpdateCustomer where
    stripeDummy _ = return $ dummyCustomer

instance StripeDummy DeleteCustomer where
    stripeDummy _ = undefined

instance StripeDummy CreateCharge where
    stripeDummy _ = undefined

instance StripeDummy GetBalanceTransaction where
    stripeDummy _ = undefined
