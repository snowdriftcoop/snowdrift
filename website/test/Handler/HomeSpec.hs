module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "browsing anonymously" $ do
        it "loads" $ do
            get HomeR
            statusIs 200
        it "has a link to /how-it-works" $ do
            get HomeR
            htmlHasLink HowItWorksR
        it "is the same as /welcome" $ do
            Just homeContents <- get HomeR >> getResponse
            Just welcomeContents <- get WelcomeR >> getResponse
            assertEqual "Contents differ" homeContents welcomeContents
    describe "browsing while logged in" $ do
        it "loads" $ do
            dummyLogin
            get HomeR
            statusIs 200
        it "is the same as /dashboard" $ do
            dummyLogin
            Just homeContents <- get HomeR >> getResponse
            Just dashboardContents <- get DashboardR >> getResponse
            assertEqual "Contents differ" homeContents dashboardContents
