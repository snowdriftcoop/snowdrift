module Model.ViewType.Internal where

import Prelude

import Database.Persist.TH (derivePersistField)

data ViewType
    = ViewApplications
    deriving (Bounded, Enum, Eq, Show, Read)
derivePersistField "ViewType"
