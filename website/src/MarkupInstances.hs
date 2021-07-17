{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Orphan markup instances
module MarkupInstances () where

import Prelude

import Data.Monoid ((<>))
import Crowdmatch
import Data.Time
import Text.Blaze.Html

dollarsCents :: Cents -> (Int, Int)
dollarsCents (Cents i) = fromIntegral i `divMod` 100

dollarsCentsTenths :: DonationUnits -> (Int, Int, Int)
dollarsCentsTenths (DonationUnits u) = (dollars, cents, fromIntegral tenths)
  where
    (totalCents, tenths) = fromIntegral u `divMod` 10
    (dollars, cents) = dollarsCents totalCents

instance ToMarkup Cents where
    toMarkup = renderDollars . dollarsCents

instance ToMarkup DonationUnits where
    toMarkup = renderDollarsFractional . dollarsCentsTenths

renderDollars :: (Int, Int) -> Html
renderDollars (0,c) = toHtml c <> "¢"
renderDollars (d,c) = "$" <> toHtml d <> "." <> padded c
  where
    padded n
        | n < 10    = "0" <> toHtml n
        | otherwise = toHtml n

renderDollarsFractional :: (Int, Int, Int) -> Html
renderDollarsFractional  (0,c,t) = toHtml c <> "." <> toHtml t <> "¢"
renderDollarsFractional  (d,c,t) = "$" <> toHtml d <> "." <> padded c <> toHtml t
  where
    padded n
        | n < 10    = "0" <> toHtml n
        | otherwise = toHtml n

instance ToMarkup HistoryTime where
    toMarkup (HistoryTime t) =
        toMarkup (formatTime defaultTimeLocale "%Y-%m-%d %H:%M (%Z)" t)

instance ToMarkup CrowdmatchDay where
    toMarkup (CrowdmatchDay d) = toMarkup (showGregorian d)
