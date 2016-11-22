{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Orphan markup instances
module MarkupInstances () where

import Prelude

import Crowdmatch
import Data.Time
import Formattable
import Text.Blaze.Html

instance ToMarkup Cents where
    toMarkup = toMarkup . formatNum usdFmt {_nfUnits = 100}

instance ToMarkup DonationUnits where
    toMarkup = toMarkup . formatNum usdFmt {_nfUnits = 1000, _nfPrec = Just (3, Decimals)}

instance ToMarkup HistoryTime where
    toMarkup (HistoryTime t) =
        toMarkup (formatTime defaultTimeLocale "%Y-%m-%d %H:%M (%Z)" t)

-- instance ToMarkup CrowdmatchDay where
--     toMarkup (CrowdmatchDay d) = toMarkup (showGregorian d)
