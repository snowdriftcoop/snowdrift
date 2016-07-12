module HandlerSpec (spec) where

import TestImport

import Network.Wai.Test (SResponse(..))

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
                Just homeResponse <- get HomeR >> getResponse
                Just welcomeResponse <- get WelcomeR >> getResponse
                assertEqualContents homeResponse welcomeResponse
        describe "browsing while logged in" $ do
            it "loads" $ do
                dummyLogin
                printBody
                get HomeR
                statusIs 200
            it "is the same as /dashboard" $ do
                dummyLogin
                get HomeR -- clear login alert
                Just homeResponse <- get HomeR >> getResponse
                Just dashboardResponse <- get DashboardR >> getResponse
                assertEqualContents homeResponse dashboardResponse
    describe "getDashboardR" $ do
        it "requires login" $
            needsAuth DashboardR "GET"

assertEqualContents :: SResponse -> SResponse -> YesodExample site ()
assertEqualContents a b = assertEqualOn simpleBody "Contents differ" a b

assertEqualOn :: Eq a => (t -> a) -> String -> t -> t -> YesodExample site ()
assertEqualOn f msg a b =
    assertEqual msg (f a) (f b)
