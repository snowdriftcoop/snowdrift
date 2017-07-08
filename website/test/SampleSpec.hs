module SampleSpec (spec_sample) where

import TestImport

spec_sample :: Spec
spec_sample = withApp $ do
    describe "various properties" $ do
        it "/home links to /privacy and /terms" $ do
            get WelcomeR
            traverse_ htmlHasLink [PrivacyR, TermsR]
