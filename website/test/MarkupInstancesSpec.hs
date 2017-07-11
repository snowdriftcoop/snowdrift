{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module MarkupInstancesSpec where

import Data.Function (on)
import Test.Tasty.HUnit
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

import MarkupInstances ()
import Crowdmatch

shouldBeHtml :: (ToMarkup m) => m -> Html -> Assertion
shouldBeHtml actual expected =
    (assertEqual "" `on` renderHtml) expected (toHtml actual)

unit_cents_8 = Cents 8 `shouldBeHtml` "8¢"
unit_cents_32 = Cents 32 `shouldBeHtml` "32¢"
unit_cents_100 = Cents 100 `shouldBeHtml` "$1.00"
unit_cents_101 = Cents 101 `shouldBeHtml` "$1.01"
unit_cents_1030 = Cents 1030 `shouldBeHtml` "$10.30"
-- TODO thousands separator
unit_cents_123456 = Cents 123456 `shouldBeHtml` "$1234.56"

unit_donationUnits_8 = DonationUnits 8 `shouldBeHtml` "0¢"
unit_donationUnits_32 = DonationUnits 32 `shouldBeHtml` "3¢"
unit_donationUnits_100 = DonationUnits 100 `shouldBeHtml` "10¢"
unit_donationUnits_101 = DonationUnits 101 `shouldBeHtml` "10¢"
unit_donationUnits_1030 = DonationUnits 1030 `shouldBeHtml` "$1.03"
unit_donationUnits_123456 = DonationUnits 123456 `shouldBeHtml` "$123.45"
