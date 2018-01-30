{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Crowdmatch.Stripe where

import Data.Aeson.Types
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text as T
import Web.Stripe hiding (stripe)
import qualified Web.Stripe as S (stripe)
import Web.Stripe.Balance (GetBalanceTransaction)
import Web.Stripe.Charge (CreateCharge)
import Web.Stripe.Customer ( Customer (..)
                           , CreateCustomer
                           , UpdateCustomer
                           , DeleteCustomer
                           , CustomerId (..)
                           , MetaData (..)
                           , StripeList (..))

-- newtype StripeState = StripeState { lastCharge :: Maybe Cents } deriving (Eq, Show)

--class StripeDummyConst a where
--    stripeDummyConst :: StripeReturn a
--
--class StripeDummyPure a where
--    stripeDummyPure :: StripeRequest a -> StripeReturn a

class StripeDummyIO a where
    stripeDummyIO :: StripeRequest a -> IO (StripeReturn a)

stripe :: (FromJSON (StripeReturn a), StripeDummyIO a)
       => StripeConfig
       -> StripeRequest a
       -> IO (Either StripeError (StripeReturn a))
stripe c s 
    | (secretKey c) == StripeKey "test" = Right <$> stripeDummyIO s
    | otherwise                         = S.stripe c s

instance StripeDummyIO CreateCustomer where
    stripeDummyIO _ = return $ dummyCustomer

instance StripeDummyIO UpdateCustomer where
    stripeDummyIO _ = return $ dummyCustomer

instance StripeDummyIO DeleteCustomer where
    stripeDummyIO _ = undefined

instance StripeDummyIO CreateCharge where
    stripeDummyIO _ = undefined

instance StripeDummyIO GetBalanceTransaction where
    stripeDummyIO _ = undefined


dummyCustomer :: Customer
dummyCustomer = Customer T.empty
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
