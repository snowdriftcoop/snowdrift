{-# LANGUAGE FlexibleInstances #-}

module Foundation where

import Prelude
import Yesod hiding ((==.), count, Value)
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.HashDB (authHashDB, setPassword)

import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)

import Model.Currency

import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad
import Control.Exception.Lifted (throwIO, handle)

import Data.Int (Int64)
import Data.Text as T

import Data.Char (isSpace)

import Data.Maybe

import Web.Authenticate.BrowserId (browserIdJs)

import Blaze.ByteString.Builder.Char.Utf8 (fromText)

import Yesod.Form.Jquery

import Yesod.Markdown (Markdown (..))

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy.Encoding as E

import Data.Time

import Database.Esqueleto

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy       as TL
import Data.Monoid

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appNavbar :: WidgetT App IO ()
    , settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }


plural :: Integral i => i -> Text -> Text -> Text
plural 1 x _ = x
plural _ _ y = y

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

licenseText :: LB.ByteString
licenseText = E.encodeUtf8 $ renderJavascriptUrl (\ _ _ -> T.empty) [julius|
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
        muser_id <- maybeAuthId
        muser <- runDB $ case muser_id of
            Nothing -> return Nothing
            Just user_id -> get user_id

        let navbar = appNavbar master
        let userPrintName :: Entity User -> Text
            userPrintName (Entity user_id user) =
                fromMaybe (either (error . T.unpack) (T.append "user") $ fromPersistValue $ unKey user_id) (userName user)
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_bootstrap_min_js
            navbar
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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

    errorHandler (PermissionDenied _) = do
        maybe_user <- maybeAuth
        selectRep $ 
            provideRep $ defaultLayout $ do
                setTitle "Permission Denied"
                toWidget [hamlet|$newline never
                    <h1>Permission Denied
                    <p>
                        $maybe _ <- maybe_user
                            You do not have permission to view this page at this time. #
                            If you think you should, let us know #
                            $# TODO
                            and we'll fix it for you or everyone. #
                            Otherwise, you can always go to our #
                            <a href="@{HomeR}">main page
                            .
                        $nothing
                            You are not logged in, and this page is not publically visible. #
                            <a href="@{AuthR LoginR}">Log in or create an account #
                            or return to our #
                            <a href="@{HomeR}">main page
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
            let license = either Left (Right . LB.append licenseText)
             in addStaticContentExternal (license . minifym) base64md5 Settings.staticDir (StaticR . flip StaticRoute []) extension mime (LB.append licenseText content)

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level = development || level `elem` [LevelInfo, LevelWarn, LevelError]

    isAuthorized _ _ = return Authorized -- restricted in the individual handlers

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

-- set which project in the site runs the site itself
getSiteProject :: Handler (Entity Project)
getSiteProject = do
    handle <- getSiteProjectHandle
    project <- runDB $ getBy $ UniqueProjectHandle handle
    case project of
         Nothing -> error "No project has been defined as the owner of this website."
         Just a -> return a

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
                <p>
                    <strong>Mozilla Persona is a secure log-in that doesn't track you! 
                    ^{parentLogin}
                <p>
                    The Persona sign-in button works for both new and existing accounts.
            |]
     in auth { apLogin = login }

snowdriftAuthHashDB :: AuthPlugin App
snowdriftAuthHashDB =
    let auth = authHashDB (Just . UniqueUser)
        loginRoute = PluginR "hashdb" ["login"]
        login toMaster =
            [whamlet|
                <div id="login">
                    <p .h3 .text-center> We also offer a built-in system
                        <br>
                        <small>
                            <a href="@{UserCreateR}">
                                click here to create a new account
                    <form .form-horizontal method="post" action="@{toMaster loginRoute}">
                        <div .form-group>
                            <label .col-sm-4 .control-label>
                                Username:
                            <div .col-sm-8>
                                <input .form-control id="x" name="username" autofocus="" required>
                        <div .form-group>
                            <label .col-sm-4 .control-label>
                                Passphrase:
                            <div .col-sm-8>
                                <input .form-control type="password" name="password" required>
                        <figure>
                            <input type="submit" value="Log in">
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
        case maybe_user_id of
            Just (Entity user_id _) -> return $ Just user_id
            Nothing -> createUser (credsIdent creds) Nothing Nothing Nothing Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ snowdriftAuthBrowserId, snowdriftAuthHashDB ]

    authHttpManager = httpManager

    loginHandler = do
        app <- lift getYesod
        toParent <- getRouteToParent

        lift $ defaultLayout $(widgetFile "auth")


createUser :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler (Maybe UserId)
createUser ident passwd name avatar nick = do
    now <- liftIO getCurrentTime
    handle (\ DBException -> return Nothing) $ runDB $ do
        account_id <- insert $ Account 0
        user <- maybe return setPassword passwd $ User ident (Just now) Nothing Nothing name account_id avatar Nothing Nothing nick now now now now Nothing Nothing
        uid_maybe <- insertUnique user
        Entity snowdrift_id _ <- getBy404 $ UniqueProjectHandle "snowdrift"
        case uid_maybe of
            Just user_id -> do
    
                -- TODO refactor back to insertSelect when quoting issue is resolved
                --
                -- insertSelect $ from $ \ p -> return $ TagColor <# (p ^. DefaultTagColorTag) <&> val user_id <&> (p ^. DefaultTagColorColor)
                --
                default_tag_colors <- select $ from return
                forM_ default_tag_colors $ \ (Entity _ (DefaultTagColor tag color)) -> insert $ TagColor tag user_id color
                --

                let message_text = Markdown $ T.unlines
                        [ "Thanks for registering!"
                        , "<br> Please read our [**welcome message**](/p/snowdrift/w/welcome), and let us know any questions."
                        ]
                -- TODO: change snowdrift_id to the generated site-project id
                void $ insert $ Message (Just snowdrift_id) now Nothing (Just user_id) message_text True
                return $ Just user_id
            Nothing -> do
                lift $ addAlert "danger" "E-mail or handle already in use."
                throwIO DBException


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
        <div class="alert alert-#{level}">
            <em>
                #{em}
            #{msg}
    |] render

addAlert :: Text -> Text -> Handler ()
addAlert level msg = do
    render <- getUrlRenderParams
    prev <- lookupSession alertKey

    setSession alertKey $ maybe id mappend prev $ TL.toStrict $ renderHtml $ [hamlet|
        $newline never
        <div class="alert alert-#{level}">
            #{msg}
    |] render

getAlert :: Handler (Maybe Html)
getAlert = do
    mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession alertKey
    deleteSession alertKey
    return mmsg

