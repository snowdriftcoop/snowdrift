{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Core definitions for the library
module Crowdmatch.Model where

import Data.Time
import Database.Persist.TH
import Web.Stripe.Customer (CustomerId(..))

import Crowdmatch.ModelDataTypes

share [mkPersist sqlSettings
      , mkMigrate "migrateCrowdmatch"
      ] [persistLowerCase|
Patron sql="crowdmatch__patron"
    usr Int
    created UTCTime
    stripeCustomer CustomerId Maybe
    donationPayable DonationUnits
    pledgeSince UTCTime Maybe
    --
    UniquePatron usr

PledgeHistory sql="crowdmatch__pledge_history"
    patron PatronId
    time UTCTime
    action PledgeAction
    --
    deriving (Show)

DonationHistory sql="crowdmatch__donation_history"
    patron PatronId
    time DonationTime
    amount DonationUnits
    fee Cents
    --
    deriving (Show)

CrowdmatchHistory sql="crowdmatch__crowdmatch_history"
    patron PatronId
    date CrowdmatchDay
    amount DonationUnits
    --
    deriving (Show)
|]
