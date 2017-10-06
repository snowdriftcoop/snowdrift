{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Define types used in mkPersist, to avoid the GHC stage restriction.
module Crowdmatch.ModelDataTypes where

import Data.Int (Int32)
import Data.Time (UTCTime, Day)
import Database.Persist.Sql
        (PersistField, PersistFieldSql)
import Database.Persist.TH (derivePersistField)
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

data StorageAction = Create | Delete deriving (Show, Read, Eq)

{-
 - This is what StorageAction would look like as a native Postgres enum
 - type. I'll wait for Persistent to support enums directly, with
 - migrations etc built in.

instance PersistField StorageAction where
    toPersistValue = \case
        Create -> PersistDbSpecific "create"
        Delete -> PersistDbSpecific "delete"

    fromPersistValue (PersistDbSpecific x)
        | x == "create" = Right Create
        | x == "delete" = Right Delete
        | otherwise = Left "Unknown enum value in StorageAction"
    fromPersistValue _ =
        Left "PledgeValue must be converted from PersistDbSpecific"

instance PersistFieldSql StorageAction where
    sqlType _ = SqlOther "storage_action"
-}

derivePersistField "StorageAction"

-- | Sanity wrapper
newtype HistoryTime = HistoryTime UTCTime deriving (PersistFieldSql, PersistField, Show)

-- | Sanity wrapper
newtype CrowdmatchDay = CrowdmatchDay Day deriving (PersistFieldSql, PersistField, Show, Eq)

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
-- whatever is a ToCrowdmatchPatron.
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
