{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TestImport
    ( runDB
    , Spec
    , Example
    , needsLogin
    , login
    , liftIO
    , extractLocation
    , module TestImport
    ) where

import Yesod (Yesod, RedirectUrl)
import Yesod.Test as TestImport
import Database.Persist as TestImport hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Network.URI (URI (uriPath), parseURI)
import Network.HTTP.Types (StdMethod (..), renderStdMethod)
import Network.Wai.Test (SResponse (..))

import qualified Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Foundation as TestImport
import Model as TestImport


type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool


-- Adjust as necessary to the url prefix in the Testing configuration
testRoot :: Text
testRoot = "http://localhost:3000/"

-- Force failure by swearing that black is white, and pigs can fly...
assertFailure :: String -> YesodExample site ()
assertFailure msg = assertEqual msg True False

-- Convert an absolute URL (eg extracted from responses) to just the path
-- for use in test requests.
urlPath :: Text -> Text
urlPath = pack . maybe "" uriPath . parseURI . unpack

-- Stages in login process, used below
firstRedirect :: (Yesod site, RedirectUrl site url) => StdMethod -> url -> YesodExample site (Maybe B.ByteString)
firstRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url

    extractLocation  -- We should get redirected to the login page

assertLoginPage :: Yesod site => Text -> YesodExample site ()
assertLoginPage loc = do
    assertEqual "correct login redirection location"
                (testRoot `T.append` "/auth/login") loc

    get $ urlPath loc
    statusIs 200
    bodyContains "Login"


submitLogin :: Yesod site => Text -> Text -> YesodExample site ()
submitLogin user pass = do
    -- Ideally we would extract this url from the login form on the current page
    request $ do
        setMethod "POST"
        setUrl $ urlPath testRoot `T.append` "auth/page/hashdb/login"
        addPostParam "username" user
        addPostParam "password" pass

    extractLocation >>= liftIO . print

extractLocation :: YesodExample site (Maybe B.ByteString)
extractLocation = do
    statusIs 303
    withResponse ( \ SResponse { simpleHeaders = h } ->
                        return $ lookup "Location" h
                 )

-- Check that accessing the url with the given method requires login, and
-- that it redirects us to what looks like the login page.
--
needsLogin :: (RedirectUrl site url, Yesod site) => StdMethod -> url -> YesodExample site ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") (assertLoginPage . decodeUtf8) mbloc

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
login :: (Yesod site) => YesodExample site ()
login = do
    liftIO $ putStrLn "Logging in..."
    get $ urlPath $ testRoot `T.append` "/auth/login"
    statusIs 200
    liftIO $ putStrLn "Submitting login."
    submitLogin "test" "test"

    liftIO $ putStrLn "Logged in."

