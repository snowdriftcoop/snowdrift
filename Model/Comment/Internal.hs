module Model.Comment.Internal where

import Prelude

import Database.Persist.TH

data ClosureType
    = Retracted
    | Closed
    deriving (Read, Show)

derivePersistField "ClosureType"

data FlagReason
    = FlagPersonalAttack
    | FlagUnconstructiveCriticism
    | FlagCondescension
    | FlagDefensiveness
    | FlagSpamming
    | FlagPrivacyViolation
    | FlagHateSpeech
    deriving (Eq, Read, Show)

derivePersistField "FlagReason"
