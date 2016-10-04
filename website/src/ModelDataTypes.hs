-- | Define types used in mkPersist, to avoid the GHC stage restriction.
module ModelDataTypes where

import ClassyPrelude

import Data.Time
import Database.Persist.Sql
import Text.Blaze.Html

data PledgeAction = CreatePledge | DeletePledge deriving (Read, Show)

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

newtype DonationTime = DonationTime UTCTime deriving (PersistFieldSql, PersistField, Show)

newtype CrowdmatchDay = CrowdmatchDay Day deriving (PersistFieldSql, PersistField, Show)

-- | Represents a donation amount. 1 DonationUnit == 0.001 US Dollars.
newtype DonationUnit = DonationUnit Int32 deriving (PersistFieldSql, PersistField, Show)

--
-- TODO: ToMarkup instances should probably go somewhere else, since that
-- is a visualization concern.
--

instance ToMarkup DonationTime where
    toMarkup (DonationTime t) =
        toMarkup (formatTime defaultTimeLocale "%Y-%m-%d %H:%M (%Z)" t)

instance ToMarkup CrowdmatchDay where
    toMarkup (CrowdmatchDay d) = toMarkup (showGregorian d)
