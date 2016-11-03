{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Define types used in mkPersist, to avoid the GHC stage restriction.
module Crowdmatch.ModelDataTypes where

import Data.Int
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Web.Stripe.Customer (CustomerId(..))

-- | Some standalone derivations so we can put CustomerId in the DB.
deriving instance PersistField CustomerId
deriving instance PersistFieldSql CustomerId

-- | Sanity wrapper. This may also eventually parameterize the different
-- payment methods, too.
newtype PaymentToken = PaymentToken { unPaymentToken :: CustomerId }
    deriving (Eq, Show)

deriving instance PersistField PaymentToken
deriving instance PersistFieldSql PaymentToken

data PledgeAction = CreatePledge | DeletePledge deriving (Show, Read, Eq)

{-
 - This is what PledgeAction would look like as a native Postgres enum
 - type. I'll wait for Persistent to support enums directly, with
 - migrations etc built in.

instance PersistField PledgeAction where
    toPersistValue = \case
        CreatePledge -> PersistDbSpecific "create_pledge"
        DeletePledge -> PersistDbSpecific "delete_pledge"

    fromPersistValue (PersistDbSpecific x)
        | x == "create_pledge" = Right CreatePledge
        | x == "delete_pledge" = Right DeletePledge
        | otherwise = Left "Unknown enum value in PledgeAction"
    fromPersistValue _ =
        Left "PledgeValue must be converted from PersistDbSpecific"

instance PersistFieldSql PledgeAction where
    sqlType _ = SqlOther "pledge_action"
-}

derivePersistField "PledgeAction"

-- | Sanity wrapper
newtype DonationTime = DonationTime UTCTime deriving (PersistFieldSql, PersistField, Show)

-- | Sanity wrapper
newtype CrowdmatchDay = CrowdmatchDay Day deriving (PersistFieldSql, PersistField, Show)

-- | Represents a donation amount. 1 DonationUnits == 0.001 US Dollars.
newtype DonationUnits = DonationUnits Int32
    deriving
        ( PersistFieldSql
        , PersistField
        , Show
        , Num
        , Integral
        , Enum
        , Real
        , Ord
        , Eq)

-- | Sanity wrapper
newtype Cents = Cents Int32
    deriving
        ( PersistFieldSql
        , PersistField
        , Show
        , Num
        , Integral
        , Enum
        , Real
        , Ord
        , Eq)

-- | "Patron pointer". This keeps track of the link between a Patron and
-- whatever is a ToMechPatron.
newtype PPtr = PPtr Int
    deriving
        ( PersistFieldSql
        , PersistField
        , Show
        , Num
        , Integral
        , Enum
        , Real
        , Ord
        , Eq)
