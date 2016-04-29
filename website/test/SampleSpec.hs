module SampleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "various properties" $ do
        it "/home links to /privacy and /terms" $ do
            get HomeR
            traverse_ htmlHasLink [PrivacyR, TermsR]
