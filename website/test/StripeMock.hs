{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Mock Stripe (mercilessly)
module StripeMock where

import ClassyPrelude.Yesod hiding (method)

import Data.Typeable
import Web.Stripe
import Web.Stripe.Error
import Test.Hspec (shouldBe)

data StripeMockEffects
    = forall a. Typeable (StripeReturn a)
    => StripeMockEffects
        { stripeReq :: StripeRequest a
        , stripeRet :: Either StripeError (StripeReturn a)
        }

{-# WARNING mockStripe "This does not work. See comments on https://git.snowdrift.coop/sd/snowdrift/merge_requests/21" #-}
-- | Ideally this could be used to mock Stripe effects, but it does not work
-- yet.
mockStripe
    :: (Typeable (StripeReturn b))
    => MVar StripeMockEffects
    -> StripeConfig
    -> StripeRequest b
    -> IO (Either StripeError (StripeReturn b))
mockStripe mocks _ actualReq = do
    (StripeMockEffects expectedReq res) <- takeMVar mocks
    method actualReq `shouldBe` method expectedReq
    endpoint actualReq `shouldBe` endpoint expectedReq
    queryParams actualReq `shouldBe` queryParams expectedReq
    fromMaybe (error "Stripe return types don't match in mock") (cast res)
