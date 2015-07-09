module Foundation where

import           Model
import           Model.Currency
import           Model.Established.Internal         (Established(..))
import           Model.Notification.Internal        (UserNotificationType(..), UserNotificationDelivery(..))
import           Model.SnowdriftEvent.Internal
import           Model.Language
import qualified Settings
import           Settings                           (widgetFile, Extra (..))
import           Settings.Development               (development)
import           Settings.StaticFiles

import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception.Lifted           (throwIO, handle)
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer.Strict        (WriterT, runWriterT)
import qualified Data.ByteString.Lazy.Char8         as LB
import           Data.Char                          (isSpace)
import           Data.Int                           (Int64)
import           Data.Maybe                         (mapMaybe, fromMaybe)
import           Data.Monoid
import           Data.Time
import           Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.Encoding            as E
import           Data.Text.Titlecase
import           Database.Esqueleto
import qualified Database.Persist
import           Network.HTTP.Conduit               (Manager)
import           Prelude
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import           Text.Hamlet                        (hamletFile)
import           Text.Jasmine                       (minifym)
import           Web.Authenticate.BrowserId         (browserIdJs)
import           Yesod                              hiding (runDB, (==.), count, Value)
import qualified Yesod                              as Y
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.HashDB                  (authHashDB, setPassword)
import           Yesod.Core.Types                   (Logger)
import           Yesod.Default.Config
import           Yesod.Default.Util                 (addStaticContentExternal)
import           Yesod.Form.Jquery
import           Yesod.Markdown                     (Markdown (..))
import           Yesod.Static

-- A type for running DB actions outside of a Handler.
type Daemon a = ReaderT App (LoggingT (ResourceT IO)) a

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appNavbar        :: WidgetT App IO ()
    , settings         :: AppConfig DefaultEnv Extra
    , getStatic        :: Static -- ^ Settings for static file serving.
    , connPool         :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager      :: Manager
    , persistConfig    :: Settings.PersistConf
    , appLogger        :: Logger
    , appEventChan     :: TChan SnowdriftEvent
    , appEventHandlers :: AppConfig DefaultEnv Extra
                       -> [SnowdriftEvent -> Daemon ()]
    }

plural :: Integral i => i -> Text -> Text -> Text
plural 1 x _ = x
plural _ _ y = y

snowdriftTitle :: MonadWidget m => Text -> m ()
snowdriftTitle t = setTitle $
    (toHtml $ titlecase $ toLower $ t) <>
    (toHtml (" | Snowdrift.coop" :: Text))

snowdriftDashTitle :: MonadWidget m => Text -> Text -> m ()
snowdriftDashTitle x y = snowdriftTitle $ x <> " — " <> y

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

licenseNotice :: LB.ByteString
licenseNotice = E.encodeUtf8 $ renderJavascriptUrl (\_ _ -> T.empty) [julius|
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
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (48 * 60)    -- timeout in minutes
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
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_bootstrap_min_js
            navbar
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s

    urlRenderOverride _ ToUR = Just (fromText "/tou")
    urlRenderOverride _ PrivacyR = Just (fromText "/priv")
    urlRenderOverride _ PostLoginR = Just (fromText "/dest")
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    errorHandler (PermissionDenied s) = do
        maybe_user <- maybeAuth
        selectRep $
            provideRep $ defaultLayout $ do
                snowdriftTitle $ "Permission Denied: " <> s
                toWidget [hamlet|$newline never
                    <h1>Permission Denied
                    <p>
                        $maybe _ <- maybe_user
                            You do not have permission to view this page at this time. #
                            If you think you should, let us know #
                            $# TODO
                            and we'll fix it for you or everyone. #
                            Otherwise, you can always go to our #
                            <a href=@{HomeR}>main page
                            .
                        $nothing
                            You are not logged in, and this page is not publicly visible. #
                            <a href=@{AuthR LoginR}>Log in or create an account
                            \or return to our #
                            <a href=@{HomeR}>main page
                            .
                |]

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

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level = development || level `elem` [LevelInfo, LevelWarn, LevelError]

    isAuthorized _ _ = return Authorized -- restricted in the individual handlers

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

-- set which project in the site runs the site itself
getSiteProject :: Handler (Entity Project)
getSiteProject = fromMaybe (error "No project has been defined as the owner of this website.") <$>
    (getSiteProjectHandle >>= runYDB . getBy . UniqueProjectHandle)

getSiteProjectHandle :: Handler Text
getSiteProjectHandle = extraSiteProject . appExtra . settings <$> getYesod

authBrowserIdFixed :: AuthPlugin App
authBrowserIdFixed =
    let complete = PluginR "browserid" []
        login :: (Route Auth -> Route App) -> WidgetT App IO ()
        login toMaster = do
            addScriptRemote browserIdJs

            toWidget [julius|
                function persona_login() {
                    navigator.id.request(
                        { siteName: null
                        // , siteLogo: '/static/img/logo.png'
                        , termsOfService: '@{ToUR}'
                        , privacyPolicy: '@{PrivacyR}'
                        , returnTo: '@{PostLoginR}'
                        , oncancel: function() {}
                        }
                    );
                }

                navigator.id.watch(
                    { loggedInUser : null
                    , onlogin :
                        function(a) {
                            if(a) document.location='@{toMaster complete}/' + a;
                            navigator.id.logout();
                        }
                    , onlogout : function() {}
                    }
                )
            |]

            toWidget [hamlet|
                $newline never
                <figure>
                  <a href="javascript:persona_login()">
                    <img src="https://browserid.org/i/persona_sign_in_blue.png">
            |]

     in (authBrowserId def) { apLogin = login }

snowdriftAuthBrowserId :: AuthPlugin App
snowdriftAuthBrowserId =
    let auth = authBrowserIdFixed
        login toMaster = do
            let parentLogin = apLogin auth toMaster
            [whamlet|
                <div .text-center>
                  <strong>
                    We support Mozilla Persona — a universal,
                    secure log-in that doesn't track you!
                ^{parentLogin}
                <p>
                  The Persona sign-in button works for
                  both new and existing accounts.
            |]
     in auth { apLogin = login }

snowdriftAuthHashDB :: AuthPlugin App
snowdriftAuthHashDB =
    let auth = authHashDB (Just . UniqueUser)
        loginRoute = PluginR "hashdb" ["login"]
        login toMaster =
            [whamlet|
                <div #login>
                  <div .text-center>
                    <strong>
                      We also offer a built-in system
                    <div #new-account-button>
                      <a href=@{UserCreateR}>
                        <button>click here to create a new account
                    <p> or log-in below:
                  <form .form-horizontal
                    method=post
                    action=@{toMaster loginRoute}>
                      <div .form-group>
                        <label .col-sm-4 .control-label
                          for=handle>
                          Handle:
                        <div .col-sm-8>
                          <input .form-control
                            id=handle
                            name=username
                            autofocus
                            required>
                      <div .form-group>
                        <label .col-sm-4 .control-label
                          for=password>
                          Passphrase:
                        <div .col-sm-8>
                          <input .form-control
                            id=password
                            type=password
                            name=password
                            required>
                      <div .form-group .text-center>
                        <div #login-button>
                          <button type=submit>
                            Log in
                        <a href=@{ResetPasswordR}>
                          forgot your password?
            |]
     in auth { apLogin = login }

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = do
        maybe_user_id <- runDB $ getBy $ UniqueUser $ credsIdent creds
        case (credsPlugin creds, maybe_user_id) of
            (_, Just (Entity user_id _)) -> return $ Just user_id
            ("hashdb",    _) -> error "Used credentials that no longer exist"
            ("browserid", _) ->
                createUser (credsIdent creds) Nothing Nothing emailStuff Nothing Nothing
            _ -> error "Unhandled credentials plugin"
      where
        emailStuff = Just $ NewEmail True $ credsIdent creds

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ snowdriftAuthBrowserId, snowdriftAuthHashDB ]

    authHttpManager = httpManager

    loginHandler = do
        app <- lift getYesod
        toParent <- getRouteToParent

        lift $ defaultLayout $(widgetFile "auth")

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

class HasGithubRepo a where
    getGithubRepo :: a (Maybe Text)

instance (MonadBaseControl IO m, MonadIO m, MonadThrow m) => HasGithubRepo (HandlerT App m) where
    getGithubRepo = extraGithubRepo . appExtra . settings <$> getYesod

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- expanded session messages
-- need to use a seperate key to maintain compatability with Yesod.Auth

alertKey :: Text
alertKey = "_MSG_ALERT"


addAlertEm :: Text -> Text -> Text -> Handler ()
addAlertEm level msg em = do
    render <- getUrlRenderParams
    prev <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $ [hamlet|
        $newline never
        <div .alert .alert-#{level}>
            <em>
                #{em}
            #{msg}
    |] render

-- TODO: don't export this
addAlert :: Text -> Text -> Handler ()
addAlert level msg = do
    render <- getUrlRenderParams
    prev   <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $ [hamlet|
        $newline never
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
type DB a = DBConstraint m => SqlPersistT m a

runDB :: DBConstraint m => DB a -> m a
runDB action = do
    app <- ask
    Database.Persist.runPool (persistConfig app) action (connPool app)

-- A database action that requires the inner monad to be Handler (for example, to use
-- get404 or getBy404)
type YDB a = SqlPersistT Handler a

runYDB :: YDB a -> Handler a
runYDB = Y.runDB

-- A database action that writes [SnowdriftEvent], to be run after the transaction is complete.
type SDB a  = DBConstraint m => WriterT [SnowdriftEvent] (SqlPersistT m) a

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
