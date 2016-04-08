module Handler.DashboardSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "requires login" $
        needsAuth DashboardR "GET"
