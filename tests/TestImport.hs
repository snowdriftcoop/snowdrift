{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module TestImport (module TestImport, marked) where

import Import (pprint)
import TestImport.Internal

import Prelude hiding (exp)

import Control.Monad (unless)
import Control.Monad.Logger as TestImport
import Control.Arrow as TestImport hiding (app)

import Yesod (Yesod, RedirectUrl, Route, RenderRoute, renderRoute)
import Yesod.Test as TestImport
import Database.Esqueleto hiding (get)
import qualified Database.Esqueleto as Esqueleto
import Database.Persist as TestImport hiding (get, (==.), delete)
import Control.Monad.IO.Class as TestImport (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)

import Network.URI (URI (uriPath), parseURI)
import Network.HTTP.Types (StdMethod (..), renderStdMethod)
import Network.Wai.Test (SResponse (..))
import qualified Text.HTML.DOM as HTML
import qualified Test.HUnit as HUnit
import qualified Text.XML as XML
import qualified Network.HTTP.Types as H

import qualified Data.ByteString as B
import           Data.Int        (Int64)

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.String

import Data.Text.Encoding (decodeUtf8)
import Foundation as TestImport
import Model as TestImport hiding
    (userNotificationContent, projectNotificationContent)

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Model.Language
import Model.Notification
    ( UserNotificationType(..), UserNotificationDelivery(..)
    , ProjectNotificationType(..), ProjectNotificationDelivery(..) )
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, terminateProcess, ProcessHandle)

import Control.Monad.Trans.Control
import Control.Exception.Lifted as Lifted hiding (handle)

import Test.QuickCheck

onException :: MonadBaseControl IO m => m a -> m b -> m a
onException = Lifted.onException

type Spec = YesodSpec App
type Example = YesodExample App

newtype FileName = FileName { unFileName :: Text }

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
    statusIsResp 303
    withResponse ( \SResponse { simpleHeaders = h } ->
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
statusIsResp number = withResponse $ \SResponse { simpleStatus = s } -> do
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

    withStatus 303 True $ request $ do
        addNonce
        setMethod "POST"
        maybe (setUrl route) setUrl (M.lookup "action" $ getAttrs form)
        addPostParam "mode" "post"
        byLabel "Language" "en"
        stmts
|]

getLatestCommentId :: YesodExample App (CommentId, Bool)
getLatestCommentId = do
    [ (Value comment_id, Value approved) ] <- testDB $ select $ from $ \comment -> do
        orderBy [ desc $ comment ^. CommentId ]
        limit 1
        return (comment ^. CommentId, not_ $ isNothing $ comment ^. CommentApprovedTs)

    return (comment_id, approved)

snowdrift :: Text
snowdrift = "snowdrift"

snowdriftId :: Example ProjectId
snowdriftId =
    testDB $ fmap entityKey $ getByOrError $ UniqueProjectHandle snowdrift

getfOrError :: (Show a, Functor f)
            => (t -> f (Maybe b)) -> (t -> a) -> t -> f b
getfOrError getf ppf x =
    fromMaybe (error $ "cannot get " <> show (ppf x)) <$> getf x

getOrError :: ( PersistEntity val
              , PersistStore (PersistEntityBackend val)
              , MonadIO m, Functor m )
           => Key val -> ReaderT (PersistEntityBackend val) m val
getOrError = getfOrError Esqueleto.get id

getByOrError :: ( PersistEntity val
                , PersistUnique (PersistEntityBackend val)
                , MonadIO m, Functor m )
             => Unique val -> ReaderT (PersistEntityBackend val) m (Entity val)
getByOrError = getfOrError getBy persistUniqueToValues

errorUnlessExpected :: (Show a, Eq a) => Text -> a -> a -> Example ()
errorUnlessExpected msg expected actual =
    unless (expected == actual) $
        error $ T.unpack msg
             <> ": expected " <> show expected
             <> ", but got " <> show actual

testProperty :: Testable prop => Text -> prop -> Example ()
testProperty msg prop = do
    res <- liftIO $ verboseCheckResult prop
    case res of
        Success {} -> return ()
        err        -> error $ T.unpack msg <> ": " <> show err

newWiki :: Text -> Language -> Text -> Text -> YesodExample App ()
newWiki project language page content = do
    get200 $ NewWikiR project language page

    withStatus 200 False $ request $ do
        addNonce
        setUrl $ NewWikiR project language page
        setMethod "POST"
        byLabel "Page Content" content
        addPostParam "mode" "preview"

    withStatus 303 False $ request $ do
        addNonce
        setUrl $ NewWikiR project language page
        setMethod "POST"
        byLabel "Page Content" content
        addPostParam "mode" "post"

keyToInt64 :: PersistField a => a -> Int64
keyToInt64 k = let PersistInt64 i = toPersistValue k in i

shpack :: Show a => a -> Text
shpack = T.pack . show

editWiki :: Text -> Language -> Text -> Text -> Text -> YesodExample App ()
editWiki project language page content comment = do
    get200 $ EditWikiR project language page

    snowdrift_id <- snowdriftId
    wiki_target <- testDB $ getByOrError $ UniqueWikiTarget snowdrift_id LangEn page
    let page_id = wikiTargetPage $ entityVal $ wiki_target
    wiki_last_edit <- testDB $ getByOrError $ UniqueWikiLastEdit page_id LangEn
    let last_edit = entityVal wiki_last_edit

    withStatus 200 False $ request $ do
        addNonce
        setUrl $ WikiR project language page
        setMethod "POST"
        byLabel "Page Content" content
        byLabel "Comment" comment
        addPostParam "f1" $ shpack $ keyToInt64 $ wikiLastEditEdit last_edit
        addPostParam "mode" "preview"

    withStatus 303 False $ request $ do
        addNonce
        setUrl $ WikiR project language page
        setMethod "POST"
        byLabel "Page Content" content
        byLabel "Comment" comment
        addPostParam "mode" "post"
        addPostParam "f1" $ shpack $ keyToInt64 $ wikiLastEditEdit last_edit

establish :: UserId -> YesodExample App ()
establish user_id = [marked|
    get200 $ UserR user_id

    withStatus 303 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl $ UserEstEligibleR user_id
        byLabel "Reason" "testing"
|]

selectUserId :: Text -> SqlPersistM UserId
selectUserId ident
    = (\case []    -> error $ "user not found: " <> T.unpack ident
             [uid] -> unValue uid
             uids  -> error $ "ident " <> T.unpack ident <> " must be unique, "
                           <> "but it matches these user ids: "
                           <> (L.intercalate ", " $ map (show . unValue) uids))
  <$> (select $ from $ \u -> do
           where_ $ u ^. UserIdent ==. val ident
           return $ u ^. UserId)

userId :: NamedUser -> Example UserId
userId = testDB . selectUserId . username

acceptHonorPledge :: YesodExample App ()
acceptHonorPledge = [marked|
    withStatus 303 False $ request $ do
        setMethod "POST"
        setUrl HonorPledgeR
|]

-- Copied from 'Model.User' but without the constraint in the result.
deleteUserNotifPrefs :: UserId -> UserNotificationType -> SqlPersistM ()
deleteUserNotifPrefs user_id notif_type =
    delete $ from $ \unp ->
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
             &&. unp ^. UserNotificationPrefType ==. val notif_type

-- Copied from 'Model.User' but without the constraint in the result.
deleteProjectNotifPrefs :: UserId -> ProjectId -> ProjectNotificationType
                        -> SqlPersistM ()
deleteProjectNotifPrefs user_id project_id notif_type =
    delete $ from $ \pnp ->
        where_ $ pnp ^. ProjectNotificationPrefUser    ==. val user_id
             &&. pnp ^. ProjectNotificationPrefProject ==. val project_id
             &&. pnp ^. ProjectNotificationPrefType    ==. val notif_type

-- Copied from 'Model.User' but without the constraint in the result.
updateUserNotifPrefs :: UserId -> UserNotificationType
                     -> UserNotificationDelivery -> SqlPersistM ()
updateUserNotifPrefs user_id notif_type notif_deliv = do
    deleteUserNotifPrefs user_id notif_type
    insert_ $ UserNotificationPref user_id notif_type notif_deliv

-- Copied from 'Model.User' but without the constraint in the result.
updateProjectNotifPrefs :: UserId -> ProjectId -> ProjectNotificationType
                        -> ProjectNotificationDelivery -> SqlPersistM ()
updateProjectNotifPrefs user_id project_id notif_type notif_deliv = do
    deleteProjectNotifPrefs user_id project_id notif_type
    insert_ $ ProjectNotificationPref user_id project_id notif_type notif_deliv

-- 'forkEventHandler' sleeps for one second in between
-- runs, so some tests will fail without this delay.
withDelay :: MonadIO m => m a -> m a
withDelay action = liftIO (threadDelay 1500000) >> action

stackExec :: FilePath -> [String] -> IO ProcessHandle
stackExec app args = spawnProcess "stack" $ ["exec", "--", app] ++ args

withEmailDaemon :: FileName -> (FileName -> IO a) -> IO ()
withEmailDaemon file action = do
    withDelay $ bracket
        (stackExec
             "SnowdriftEmailDaemon"
             [ "--sendmail=stack exec SnowdriftSendmail"
             , "--sendmail-file=" <> T.unpack (unFileName file)
             , "--db=testing"
             ])
        terminateProcess
        (const $ withDelay $ void $ action file)

processPayments :: IO ()
processPayments = do
    bracket
        (stackExec
             "SnowdriftProcessPayments"
             ["Testing"])
        terminateProcess
        (const $ withDelay $ return ())

rethreadComment :: Text -> Text -> YesodExample App ()
rethreadComment rethread_route parent_route = [marked|
    get200 rethread_route

    withStatus 303 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl rethread_route
        byLabel "New Parent Url" parent_route
        byLabel "Reason" "testing"
        addPostParam "mode" "post"
|]

flagComment :: Text -> YesodExample App ()
flagComment route = [marked|
    get200 route

    withStatus 303 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        addPostParam "f1" "1"
        addPostParam "f2" ""
        addPostParam "mode" "post"
|]

editComment :: Text -> Text -> YesodExample App ()
editComment route comment_text = [marked|
    get200 route

    withStatus 303 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        byLabel "Edit" comment_text
        byLabel "Language" "en"
        addPostParam "mode" "post"
    |]

changeWatchStatus :: RedirectUrl App url => url -> YesodExample App ()
changeWatchStatus route = [marked|
     withStatus 303 False $ request $ do
         setMethod "POST"
         setUrl route
|]

newBlogPost :: Text -> YesodExample App ()
newBlogPost page = [marked|
    let route = NewBlogPostR snowdrift
    get200 route

    withStatus 303 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        byLabel "Title for this blog post" "testing"
        byLabel "Handle for the URL" page
        byLabel "Content" "testing"
        addPostParam "mode" "post"
|]

loadFunds :: UserId -> Int -> Example ()
loadFunds user_id n = [marked|
    let route = UserBalanceR user_id
    get200 route

    withStatus 303 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        addPostParam "f1" $ shpack n
    |]

pledgeStatus :: ProjectId -> Int64 -> Int -> Example ()
pledgeStatus project_id shares status = [marked|
    project <-
        testDB (Esqueleto.get project_id) >>= return .
            fromMaybe (error $ "cannot find project " <> pprint project_id)
    let handle  = projectHandle project
        tshares = shpack shares
    get200 $ ProjectR handle

    let route = UpdatePledgeR handle
    withStatus status False $ request $ do
        setUrl route
        setMethod "GET"
        addGetParam "_hasdata" ""
        addGetParam "f1" tshares

    when (status == 200) $
        withStatus 303 False $ request $ do
            addNonce
            setMethod "POST"
            setUrl route
            addPostParam "f1" tshares
            addPostParam "confirm" "yes!"
|]

pledge :: ProjectId -> Int64 -> Example ()
pledge project_id shares = pledgeStatus project_id shares 200

pledgeFail :: ProjectId -> Int64 -> Example ()
pledgeFail project_id shares = pledgeStatus project_id shares 303

named_users :: [NamedUser]
named_users = [minBound .. maxBound]

render :: RenderRoute App => Text -> Route App -> Text
render appRoot = ((appRoot <> "/") <>) . T.intercalate "/" . fst . renderRoute

enRoute :: (Text -> Language -> a) -> a
enRoute c = c snowdrift LangEn
