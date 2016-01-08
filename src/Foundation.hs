module Foundation where

import Import.NoFoundation

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Concurrent.STM
import Control.Exception.Lifted (throwIO, handle)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import Data.Char (isSpace)
import Data.Text as T
import Network.HTTP.Conduit (Manager)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Web.Authenticate.BrowserId (browserIdJs)
import Yesod hiding (runDB, (==.), count, Value)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Auth.HashDB (authHashDB, setPassword)
import Yesod.Core.Types (Logger)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery
import Yesod.Static
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Database.Persist
import qualified Settings
import qualified Yesod as Y

import Avatar
import Model.Currency

-- A type for running DB actions outside of a Handler.
type Daemon a = ReaderT App (LoggingT (ResourceT IO)) a

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appNavbar        :: WidgetT App IO ()
    , appSettings      :: AppConfig DefaultEnv Extra
    , appStatic        :: Static -- ^ Settings for static file serving.
    , appConnPool      :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , appHttpManager   :: Manager
    , persistConfig    :: Settings.PersistConf
    , appLogger        :: Logger
    , appEventChan     :: TChan SnowdriftEvent
    , appEventHandlers :: AppConfig DefaultEnv Extra
                       -> [SnowdriftEvent -> Daemon ()]
    }

plural :: Integral i => i -> Text -> Text -> Text
plural 1 x _ = x
plural _ _ y = y

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- FIXME
type ProjectHandle = Text
type UserHandle = UserId

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

licenseNotice :: LB.ByteString
licenseNotice = E.encodeUtf8 $ renderJavascriptUrl (\_ _ -> T.empty)
    [julius|
    /*
     @licstart  The following is the entire license notice for the JavaScript code in this page.

     Copyright (C) 2012-2014  Snowdrift.coop

     The JavaScript code in this page is free software: you can
     redistribute it and/or modify it under the terms of the GNU
     Affero General Public License (GNU AGPL) as published by the Free Software
     Foundation, either version 3 of the License, or (at your option)
     any later version.  The code is distributed WITHOUT ANY WARRANTY;
     without even the implied warranty of MERCHANTABILITY or FITNESS
     FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

     As additional permission under GNU AGPL version 3 section 7, you
     may distribute non-source (e.g., minimized or compacted) forms of
     that code without the copy of the GNU AGPL normally required by
     section 4, provided you include this license notice and a URL
     through which recipients can access the Corresponding Source.

     @licend  The above is the entire license notice for the JavaScript code in this page.
    */
    |]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies.
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (48 * 60)    -- timeout in minutes, (48 * 60) = 48 hours = 2 days
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        malert <- getAlert

        let navbar = appNavbar master

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            setTitle "Snowdrift.coop"
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_bootstrap_min_js
            navbar
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y = \case
        StaticR s  ->
            Just (uncurry (joinPath y (Settings.staticRoot (appSettings y)))
                          (renderRoute s))
        -- I have determined that this *only* exists for the sake of
        -- templates/persona.julius.
        PostLoginR -> Just (fromText "/dest")
        _          -> Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- default to authorized, restricted in the individual handlers
    isAuthorized _ _ = return Authorized

    errorHandler (PermissionDenied s) = do
        maybe_user <- maybeAuth
        selectRep $
            provideRep $ defaultLayout $ do
                setTitle $
                    toHtml $ "Permission Denied: " <> s <> " | Snowdrift.coop"
                $(widgetFile "permission-denied")

    errorHandler other_error = defaultErrorHandler other_error

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    --
    -- Adds license before minification and after successful minification,
    -- to make sure it's there whether or not minification succeeds.
    --
    addStaticContent extension mime content =
        if LB.all isSpace content
         then return Nothing
         else
            let license = either Left (Right . LB.append licenseNotice)
             in addStaticContentExternal (license . minifym) base64md5 Settings.staticDir (StaticR . flip StaticRoute []) extension mime (LB.append licenseNotice content)

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level = development || level `elem` [LevelInfo, LevelWarn, LevelError]

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- set which project in the site runs the site itself
getSiteProject :: Handler (Entity Project)
getSiteProject = fromMaybe (error "No project has been defined as the owner of this website.") <$>
    (getSiteProjectHandle >>= runYDB . getBy . UniqueProjectHandle)

getSiteProjectHandle :: Handler Text
getSiteProjectHandle = extraSiteProject . appExtra . appSettings <$> getYesod


snowdriftAuthBrowserId :: AuthPlugin App
snowdriftAuthBrowserId =
    let complete = PluginR "browserid" []
        login toMaster = do
            addScriptRemote browserIdJs
            $(widgetFile "auth/persona")
     in (authBrowserId def) { apLogin = login }

snowdriftAuthHashDB :: AuthPlugin App
snowdriftAuthHashDB =
    let auth = authHashDB (Just . UniqueUser)
        loginRoute = PluginR "hashdb" ["login"]
        login toMaster = do
            handleInputId <- newIdent
            passphraseInputId <- newIdent
            $(widgetFile "auth/built-in-login")
     in auth { apLogin = login }

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = do
        maybe_user_id <- runDB $ getBy $ UniqueUser $ credsIdent creds
        case (credsPlugin creds, maybe_user_id) of
            (_, Just (Entity user_id _)) -> return $ Just user_id
            ("hashdb",    _) -> error "Credentials not recognized"
            ("browserid", _) ->
                createUser (credsIdent creds) Nothing Nothing emailStuff Nothing Nothing
            _ -> error "Unhandled credentials plugin"
      where
        emailStuff = Just $ NewEmail True $ credsIdent creds

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ snowdriftAuthBrowserId, snowdriftAuthHashDB ]

    authHttpManager = appHttpManager

    loginHandler = do
        toParent <- getRouteToParent
        lift $ defaultLayoutNew "auth/login" $(widgetFile "auth/login")

instance YesodAuthPersist App

data NewEmail = NewEmail
    { neVerified :: Bool
    , neAddr :: Text
    }

createUser :: Text -> Maybe Text -> Maybe Text -> Maybe NewEmail -> Maybe Text
           -> Maybe Text -> Handler (Maybe UserId)
createUser ident passwd name newEmail avatar nick = do
    langs <- mapMaybe (readMaybe . T.unpack) <$> languages
    now <- liftIO getCurrentTime
    handle (\DBException -> return Nothing) $ runYDB $ do
        account_id <- insert (Account 0)
        discussion_id <- insert (Discussion 0)
        user <- maybe return setPassword passwd $ newUser langs now account_id discussion_id
        uid_maybe <- insertUnique user
        case uid_maybe of
            Just user_id -> do

                -- TODO: refactor back to insertSelect when quoting issue is resolved
                --
                -- insertSelect $ from $ \p -> return $ TagColor <# (p ^. DefaultTagColorTag) <&> val user_id <&> (p ^. DefaultTagColorColor)
                --
                default_tag_colors <- select $ from return
                forM_ default_tag_colors $ \(Entity _ (DefaultTagColor tag color)) -> insert $ TagColor tag user_id color
                --

                insertDefaultNotificationPrefs user_id
                welcome_route <- getUrlRender
                            -- 'MonolingualWikiR' is deprecated.
                            <*> pure (MonolingualWikiR "snowdrift" "welcome" [])
                let notif_text = Markdown $ T.unlines
                        [ "Thanks for registering!"
                        , "<br> Please read our [**welcome message**](" <>
                          welcome_route <>
                          "), and let us know any questions."
                        ]

                insert_ $ UserNotification now NotifWelcome user_id notif_text False
                return $ Just user_id
            Nothing -> do
                lift $ addAlert "danger" "Handle already in use."
                throwIO DBException
  where
    newUser langs now account_id discussion_id =
        User { userIdent = ident
             , userEmail = (neAddr <$> newEmail)
             , userEmail_verified = (maybe False neVerified newEmail)
             , userCreatedTs = now
             , userHash = Nothing
             , userSalt = Nothing
             , userName = name
             , userAccount = account_id
             , userAvatar = avatar
             , userBlurb = Nothing
             , userStatement = Nothing
             , userIrcNick = nick
             , userLanguages = langs
             , userReadNotifications = now
             , userReadApplications = now
             , userEstablished = EstUnestablished
             , userDiscussion = discussion_id
             }

    insertDefaultNotificationPrefs :: UserId -> DB ()
    insertDefaultNotificationPrefs user_id =
        void . insertMany $ uncurry (UserNotificationPref user_id) <$>
            -- 'NotifWelcome' is not set since it is delivered when a
            -- user is created.
            [ (NotifBalanceLow,        UserNotifDeliverWebsiteAndEmail)
            , (NotifUnapprovedComment, UserNotifDeliverEmail)
            , (NotifRethreadedComment, UserNotifDeliverWebsite)
            , (NotifReply,             UserNotifDeliverEmail)
            , (NotifEditConflict,      UserNotifDeliverWebsite)
            , (NotifFlag,              UserNotifDeliverWebsiteAndEmail)
            , (NotifFlagRepost,        UserNotifDeliverWebsite)
            ]

instance YesodJquery App

instance (MonadBaseControl IO m, MonadIO m, MonadThrow m) => HasGithubRepo (HandlerT App m) where
    getGithubRepo = extraGithubRepo . appExtra . appSettings <$> getYesod

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . appSettings) getYesod

-- expanded session messages
-- need to use a seperate key to maintain compatability with Yesod.Auth

alertKey :: Text
alertKey = "_MSG_ALERT"


addAlertEm :: Text -> Text -> Text -> Handler ()
addAlertEm level msg em = do
    render <- getUrlRenderParams
    prev <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $
        [hamlet|
        <div .alert .alert-#{level}>
          <em>#{em}
          #{msg}
        |] render

-- TODO: don't export this
addAlert :: Text -> Text -> Handler ()
addAlert level msg = do
    render <- getUrlRenderParams
    prev   <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $
        [hamlet|
        <div .alert .alert-#{level}>
          #{msg}
        |] render

alertDanger, alertInfo, alertSuccess, alertWarning :: Text -> Handler ()
alertDanger  = addAlert "danger"
alertInfo    = addAlert "info"
alertSuccess = addAlert "success"
alertWarning = addAlert "warning"

getAlert :: Handler (Maybe Html)
getAlert = do
    mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession alertKey
    deleteSession alertKey
    return mmsg

-- | Write a list of SnowdriftEvent to the event channel.
pushEvents :: (MonadIO m, MonadReader App m) => [SnowdriftEvent] -> m ()
pushEvents events = ask >>= liftIO . atomically . forM_ events . writeTChan . appEventChan

--------------------------------------------------------------------------------

-- There are FOUR different kinds of database actions, each with a different run function.
-- Terminology:
--    DB - database
--    Y  - Yesod
--    S  - Snowdrift

-- Convenient type synonym for all that is required to hit the database in a monad.
-- Types that satisfy this constraint: Handler, Daemon.
type DBConstraint m = (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadResource m, MonadReader App m)

-- Run a Daemon in IO.
runDaemon :: App -> Daemon a -> IO a
runDaemon app daemon =
    runResourceT $
      runLoggingT
        (runReaderT daemon app)
        (messageLoggerSource app (appLogger app))

-- A basic database action.
type DB a = forall m. DBConstraint m => SqlPersistT m a

runDB :: DBConstraint m => DB a -> m a
runDB action = do
    app <- ask
    Database.Persist.runPool (persistConfig app) action (appConnPool app)

-- A database action that requires the inner monad to be Handler (for example, to use
-- get404 or getBy404)
type YDB a = SqlPersistT Handler a

runYDB :: YDB a -> Handler a
runYDB = Y.runDB

-- A database action that writes [SnowdriftEvent], to be run after the transaction is complete.
type SDB a  = forall m. DBConstraint m => WriterT [SnowdriftEvent] (SqlPersistT m) a

runSDB :: DBConstraint m => SDB a -> m a
runSDB w = do
    (a, events) <- runDB (runWriterT w)
    pushEvents events
    return a

-- A combination of YDB and SDB (writes events, requires inner Handler).
type SYDB a = WriterT [SnowdriftEvent] (SqlPersistT Handler) a

runSYDB :: SYDB a -> Handler a
runSYDB w = do
    (a, events) <- runYDB (runWriterT w)
    pushEvents events
    return a

-- from http://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe
readMaybe   :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                  [x] -> Just x
                  _   -> Nothing

-- | New layout for new pages.
--
-- Once the new design is in place, this will probably replace
-- defaultLayout... though we may want to continue to have separate
-- 'defaults' for different sections e.g. project pages
--
-- pageName is used as a class for a top-level <div> wrapper.
defaultLayoutNew :: Text -> Widget -> Handler Html
defaultLayoutNew pageName widget = do
    master <- getYesod
    mmsg <- getMessage
    malert <- getAlert
    maybeUser  <- maybeAuth
    avatar <- getUserAvatar (StaticR img_default_avatar_png)
                            (maybe Nothing (Just . entityVal) maybeUser)

    active <- maybe (const False) (==) <$> getCurrentRoute
    howItWorksActive <- do
        r <- getCurrentRoute
        return $ case r of
            Just (HowItWorksR _) -> True
            _                    -> False
    authActive <- do
        r <- getCurrentRoute
        return $ case r of
            Just (AuthR _)        -> True
            Just ResetPassphraseR -> True
            Just CreateAccountR   -> True
            _                     -> False


    let navbar :: Widget = $(widgetFile "default/navbar")
    let footer :: Widget = $(widgetFile "default/footer")

    pc <- widgetToPageContent $ do
        $(widgetFile "default/reset")
        $(widgetFile "default/breaks")
        $(widgetFile "default/fonts")
        $(widgetFile "default/grid")
        $(widgetFile "default-layout-new")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
