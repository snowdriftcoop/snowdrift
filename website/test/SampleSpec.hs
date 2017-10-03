module SampleSpec (spec_sample) where

import TestImport

spec_sample :: Spec
spec_sample = withApp $
    describe "various properties" $
        it "/home links to /privacy and /terms" $ do
            get WelcomeR
            traverse_ htmlHasLink [PrivacyR, TermsR]
