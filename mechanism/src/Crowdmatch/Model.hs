{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Core definitions for the library
module Crowdmatch.Model (module Crowdmatch.Model) where

import Control.Lens
import Control.Lens.TH
import Data.Time
import Database.Persist.TH
import Web.Stripe.Customer (CustomerId(..))

import Crowdmatch.ModelDataTypes as Crowdmatch.Model

share [mkPersist sqlSettings
      , mkMigrate "migrateCrowdmatch"
      ] [persistLowerCase|
Patron sql="crowdmatch__patron"
    usr PPtr
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

class ToMechPatron u where
    toMechPatron :: u -> Int
    fromMechPatron :: Int -> u

external :: ToMechPatron u => Iso' PPtr u
external = iso toExternal fromExternal
  where
    toExternal (PPtr i) = fromMechPatron i
    fromExternal = PPtr . toMechPatron
