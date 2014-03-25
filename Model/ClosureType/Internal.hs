
module Model.ClosureType.Internal where

import Prelude

import Database.Persist.TH

data ClosureType = Retracted | Closed
    deriving (Read, Show)

derivePersistField "ClosureType"

