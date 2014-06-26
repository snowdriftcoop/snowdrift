module Model.Comment.Internal where

import Prelude

import Data.Text (Text)
import Database.Persist.TH

data ClosureType
    = Retracted
    | Closed
    deriving (Read, Show)

derivePersistField "ClosureType"

data FlagReason
    = FlagOffensive
    | FlagOther Text
    deriving (Read, Show)

derivePersistField "FlagReason"
