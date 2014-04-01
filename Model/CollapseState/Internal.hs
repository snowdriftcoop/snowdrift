
module Model.CollapseState.Internal where

import Prelude

import Database.Persist.TH

data CollapseState = FullyVisible | Collapsed | FullyHidden
    deriving (Read, Show, Eq, Ord)

derivePersistField "CollapseState"

