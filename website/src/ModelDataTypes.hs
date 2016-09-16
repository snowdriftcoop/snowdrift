-- | Define types used in mkPersist, to avoid the GHC stage restriction.
module ModelDataTypes where

import ClassyPrelude

import Database.Persist.TH
import Database.Persist.Sql

data PledgeAction = MakePledge | RemovePledge deriving (Read, Show)

instance PersistField PledgeAction where
    toPersistValue = \case
        MakePledge -> PersistDbSpecific "make_pledge"
        RemovePledge -> PersistDbSpecific "remove_pledge"

    fromPersistValue (PersistDbSpecific x)
        | x == "make_pledge" = Right MakePledge
        | x == "remove_pledge" = Right RemovePledge
        | otherwise = Left "Unknown enum value in PledgeAction"
    fromPersistValue _ = Left "PledgeValue must be converted from PersistDbSpecific"

instance PersistFieldSql PledgeAction where
    sqlType _ = SqlOther "pledge_action"
