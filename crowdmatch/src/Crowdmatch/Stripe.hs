{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Crowdmatch.Stripe where

import Data.Aeson.Types
import qualified Web.Stripe as S

stripe :: FromJSON (S.StripeReturn a)
       => S.StripeConfig
       -> S.StripeRequest a
       -> IO (Either S.StripeError (S.StripeReturn a))
stripe c s 
    | (S.secretKey c) == 
            S.StripeKey "test" = error "Snowdrift Stripe Test"
    | otherwise               = S.stripe c s
    
