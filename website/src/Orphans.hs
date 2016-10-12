{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where

import Crowdmatch.ModelDataTypes
import Data.Time
import Text.Blaze.Html

--
-- TODO: ToMarkup instances should probably go somewhere else, since that
-- is a visualization concern.
--

instance ToMarkup DonationTime where
    toMarkup (DonationTime t) =
        toMarkup (formatTime defaultTimeLocale "%Y-%m-%d %H:%M (%Z)" t)

instance ToMarkup CrowdmatchDay where
    toMarkup (CrowdmatchDay d) = toMarkup (showGregorian d)
