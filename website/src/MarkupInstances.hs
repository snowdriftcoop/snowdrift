{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Orphan markup instances
module MarkupInstances () where

import Prelude

import Crowdmatch
import Data.Default (def)
import Data.Text (Text)
import Data.Time
import Formattable
import Text.Blaze.Html

instance ToMarkup Cents where
    toMarkup = toMarkup . formatNum usdFmt {_nfUnits = 100}

instance ToMarkup DonationUnits where
    toMarkup = toMarkup . formatNum usdFmt {_nfUnits = 1000, _nfPrec = Just (3, Decimals)}

-- formatCents :: Cents -> Text
-- formatCents c
--     | c < 100 = formatIntegral (NumFormat 1 "" "¢" "" "" Fixed Nothing NegMinusSign) c
--     | otherwise = formatNum usdFmt {_nfUnits = 100} c

-- formatMill :: Mill -> Text
-- formatMill m
--     | m < 1000

instance ToMarkup DonationTime where
    toMarkup (DonationTime t) =
        toMarkup (formatTime defaultTimeLocale "%Y-%m-%d %H:%M (%Z)" t)

-- instance ToMarkup CrowdmatchDay where
--     toMarkup (CrowdmatchDay d) = toMarkup (showGregorian d)
