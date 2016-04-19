module HandlerSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "getRobotsR" $ do
        it "gives a 200" $ do
            get RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            get RobotsR
            bodyContains "User-agent: *"
    describe "getFaviconR" $ do
        it "gives a 200" $ do
            get FaviconR
            statusIs 200
    describe "getHomeR" $ do
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
    describe "getDashboardR" $ do
        it "requires login" $
            needsAuth DashboardR "GET"
