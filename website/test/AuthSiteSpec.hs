{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module AuthSiteSpec (spec) where

import TestImport
import Yesod hiding (get)
import Database.Persist.Sql hiding (get)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)

import AuthSite
import Application (makeFoundation, makeLogWare)

-- ** A harness for the auth subsite

data AuthHarness = AuthHarness
    { ahTestAuth :: (AuthSite (Route AuthHarness))
    , ahConnPool :: ConnectionPool
    }

mkYesod "AuthHarness" [parseRoutes|
/foo Foo GET
/auth-user TheUser GET
/auth AuthSub AuthSite-(Route-AuthHarness) ahTestAuth
|]

getFoo :: HandlerT AuthHarness IO Html
getFoo = defaultLayout [whamlet|
    Lo, it is an AuthHarness page.
|]

getTheUser :: HandlerT AuthHarness IO Html
getTheUser = do
    user :: Maybe (Entity User) <- headMay <$> runDB (selectList [] [])
    defaultLayout $ maybe
        [whamlet|No users|]
        (\(Entity _ u) -> [whamlet|#{show u}|])
        user

-- ** Boilerplate for the harness site

instance Yesod AuthHarness

instance YesodPersist AuthHarness where
    type YesodPersistBackend AuthHarness = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (ahConnPool master)

instance RenderMessage AuthHarness FormMessage where
    renderMessage _ _ = defaultFormMessage


-- ** Boilerplate to run Specs with this harness.

withTestAuth :: SpecWith (TestApp AuthHarness) -> Spec
withTestAuth = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    let at = AuthHarness (AuthSite TheUser) (appConnPool foundation)
    return (at, logWare)

-- ** The actual tests!

spec :: Spec
spec = withTestAuth $ do
    it "plarp" $ do
        get TheUser
        bodyContains "No users"
