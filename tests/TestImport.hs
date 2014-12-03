{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module TestImport (module TestImport, marked) where
import TestImport.Internal

import Prelude hiding (exp)

import Control.Monad.Logger as TestImport
import Control.Arrow as TestImport

import Yesod (Yesod, RedirectUrl)
import Yesod.Test as TestImport
import Database.Esqueleto hiding (get)
import Database.Persist as TestImport hiding (get, (==.))
import Control.Monad.IO.Class as TestImport (liftIO, MonadIO)

import Network.URI (URI (uriPath), parseURI)
import Network.HTTP.Types (StdMethod (..), renderStdMethod)
import Network.Wai.Test (SResponse (..))
import qualified Text.HTML.DOM as HTML
import qualified Test.HUnit as HUnit
import qualified Text.XML as XML
import qualified Network.HTTP.Types as H

import qualified Data.ByteString as B

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.String

import Data.Text.Encoding (decodeUtf8)
import Foundation as TestImport
import Model as TestImport

import Control.Monad (when)

import System.IO (hPutStrLn, stderr)

import Control.Monad.Trans.Control
import Control.Exception.Lifted as Lifted


onException :: MonadBaseControl IO m => m a -> m b -> m a
onException = Lifted.onException

type Spec = YesodSpec App
type Example = YesodExample App

testDB :: SqlPersistM a -> Example a
testDB query = do
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
urlPath = T.pack . maybe "" uriPath . parseURI . T.unpack

-- Stages in login process, used below
firstRedirect :: (Yesod site, RedirectUrl site url) => StdMethod -> url -> YesodExample site (Maybe B.ByteString)
firstRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url

    extractLocation  -- We should get redirected to the login page

withStatus :: Int -> Bool -> YesodExample App () -> YesodExample App ()
withStatus status resp req =
    req >> (if resp then statusIsResp else statusIs) status

get200 :: RedirectUrl App url => url -> YesodExample App ()
get200 route = withStatus 200 False $ get route

assertLoginPage :: Text -> YesodExample App ()
assertLoginPage loc = do
    assertEqual "correct login redirection location"
                (testRoot `T.append` "/auth/login") loc

    withStatus 200 True $ get $ urlPath loc
    bodyContains "Login"


submitLogin :: Yesod site => Text -> Text -> YesodExample site ()
submitLogin user pass = do
    -- Ideally we would extract this url from the login form on the current page
    request $ do
        setMethod "POST"
        setUrl $ urlPath testRoot `T.append` "auth/page/hashdb/login"
        addPostParam "username" user
        addPostParam "password" pass


extractLocation :: YesodExample site (Maybe B.ByteString)
extractLocation = do
    statusIsResp 302
    withResponse ( \ SResponse { simpleHeaders = h } ->
                        return $ lookup "Location" h
                 )

-- Check that accessing the url with the given method requires login, and
-- that it redirects us to what looks like the login page.
--
needsLogin :: RedirectUrl App url => StdMethod -> url -> YesodExample App ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") (assertLoginPage . decodeUtf8) mbloc


data NamedUser = Bob | Mary | Joe | Sue deriving (Eq, Bounded, Enum)
data TestUser = TestUser
data AdminUser = AdminUser

class Login a where
    username :: IsString name => a -> name
    password :: IsString pass => a -> pass

instance Login NamedUser where
    username Bob =  "bob"
    username Mary = "mary"
    username Joe =  "joe"
    username Sue =  "sue"

    password Bob =  "bob password"
    password Mary = "mary password"
    password Joe =  "joe password"
    password Sue =  "sue password"

instance Login TestUser where
    username _ = "test"
    password _ = "test"

instance Login AdminUser where
    username _ = "admin"
    password _ = "admin"

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
loginAs :: Login user => user -> YesodExample App ()
loginAs user = do
    get200 $ urlPath $ testRoot `T.append` "/auth/login"
    submitLogin (username user) (password user)


statusIsResp :: Int -> YesodExample site ()
statusIsResp number = withResponse $ \ SResponse { simpleStatus = s } -> do
    let errMsg = concat
            [ "Expected status was ", show number
            , " but received status was ", show $ H.statusCode s
            ]

    when (H.statusCode s /= number) $ do
        liftIO $ hPutStrLn stderr $ errMsg ++ ":"
        printBody
        liftIO $ hPutStrLn stderr ""

    liftIO $ flip HUnit.assertBool (H.statusCode s == number) errMsg

postComment :: RedirectUrl App url => url -> RequestBuilder App ()
            -> YesodExample App ()
postComment route stmts = [marked|
    get200 route

    [ form ] <- htmlQuery "form"

    let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS

    withStatus 302 True $ request $ do
        addNonce
        setMethod "POST"
        maybe (setUrl route) setUrl (M.lookup "action" $ getAttrs form)
        addPostParam "mode" "post"
        byLabel "Language" "en"
        stmts
|]

getLatestCommentId :: YesodExample App (CommentId, Bool)
getLatestCommentId = do
    [ (Value comment_id, Value approved) ] <- testDB $ select $ from $ \ comment -> do
        orderBy [ desc $ comment ^. CommentId ]
        limit 1
        return (comment ^. CommentId, not_ $ isNothing $ comment ^. CommentApprovedTs)

    return (comment_id, approved)

