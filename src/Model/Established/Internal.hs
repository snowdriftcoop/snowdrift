module Model.Established.Internal where

import Prelude

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (derivePersistField)

data Established
    = EstUnestablished
    | EstEligible      UTCTime Text
    | EstEstablished   UTCTime UTCTime Text
    deriving (Read, Show)

estIsEstablished :: Established -> Bool
estIsEstablished (EstEstablished{}) = True
estIsEstablished _                  = False

estIsEligible :: Established -> Bool
estIsEligible (EstEligible _ _) = True
estIsEligible _                 = False

estIsUnestablished :: Established -> Bool
estIsUnestablished EstUnestablished = True
estIsUnestablished _                = False

derivePersistField "Established"
