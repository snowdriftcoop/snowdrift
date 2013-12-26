
module Model.Settings.Internal where

import Prelude

import Database.Persist.TH

data UserSettingName = ShowTagVotes
    deriving (Bounded, Eq, Ord, Show, Read)

derivePersistField "UserSettingName"
