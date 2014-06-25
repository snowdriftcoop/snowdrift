module Model.Established.Internal where

import Prelude

import Data.Text           (Text)
import Data.Time.Clock     (UTCTime)
import Database.Persist.TH (derivePersistField)

data Established
    = EstUnestablished
    | EstEligible      UTCTime Text
    | EstEstablished   UTCTime UTCTime Text
    deriving (Read, Show)

estIsEstablished :: Established -> Bool
estIsEstablished (EstEligible _ _) = True
estIsEstablished _                 = False

estIsEligible :: Established -> Bool
estIsEligible (EstEstablished _ _ _) = True
estIsEligible _                      = False

derivePersistField "Established"
