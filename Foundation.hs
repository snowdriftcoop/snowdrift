{-# LANGUAGE FlexibleInstances #-}

module Foundation where

import Prelude
import Yesod hiding ((==.), count, Value)
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.HashDB (authHashDB)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)

import Model.Currency
-- import Model.Role.Internal

import Control.Applicative
import Control.Monad.Trans.Resource

import Data.Int (Int64)
import Data.Text (Text)

import Data.Char (isSpace)

import Web.Authenticate.BrowserId (browserIdJs)

import Blaze.ByteString.Builder.Char.Utf8 (fromText)

import Yesod.Form.Jquery

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text as T

import Data.Time

import Database.Esqueleto

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
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
-- http://www.yesodweb.com/book/handler
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

     Copyright (C) 2012  Snowdrift.coop

     The JavaScript code in this page is free software: you can
     redistribute it and/or modify it under the terms of the GNU
     General Public License (GNU GPL) as published by the Free Software
     Foundation, either version 3 of the License, or (at your option)
     any later version.  The code is distributed WITHOUT ANY WARRANTY;
     without even the implied warranty of MERCHANTABILITY or FITNESS
     FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

     As additional permission under GNU GPL version 3 section 7, you
     may distribute non-source (e.g., minimized or compacted) forms of
     that code without the copy of the GNU GPL normally required by
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
        (48 * 60 * 60)
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_bootstrap_min_js
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s

    urlRenderOverride _ TosR = Just (fromText "/tos")
    urlRenderOverride _ PrivacyR = Just (fromText "/priv")
    urlRenderOverride _ PostLoginR = Just (fromText "/dest")
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    errorHandler (PermissionDenied _) = do
        maybe_user <- maybeAuth
        selectRep $ do
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
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    isAuthorized _ _ = return Authorized -- restricted in the individual handlers


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

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
                        , termsOfService: '@{TosR}'
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
                <p>
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
                    <strong>Mozilla Persona is a secure and private log-in system that requires no new password.
                <p>
                    Persona doesn't track you the way other log-in systems do,
                    and it works directly with gmail and yahoo accounts or with any e-mail after verification.
                <p>
                    The Sign in button below works for both new or existing snowdrift.coop accounts:
                ^{parentLogin}
            |]
     in auth { apLogin = login }

snowdriftAuthHashDB :: AuthPlugin App
snowdriftAuthHashDB =
    let auth = authHashDB (Just . UniqueUser)
        login toMaster = do
            let parentLogin = apLogin auth toMaster
            [whamlet|
                <p> We also offer a built-in traditional log-in system:
                <p>
                    <a href="@{UserCreateR}">
                       click here to create an account
                <p> or
                ^{parentLogin}
            |]
     in auth { apLogin = login }

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        now <- liftIO getCurrentTime
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                account_id <- insert $ Account $ Milray 0
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing Nothing account_id Nothing Nothing Nothing Nothing now now now now

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ snowdriftAuthBrowserId, snowdriftAuthHashDB ]

    authHttpManager = httpManager

    loginHandler = do
        app <- lift getYesod
        toParent <- getRouteToParent

        lift $ defaultLayout $(widgetFile "auth")



instance YesodJquery App

class HasGithubRepo a where
    getGithubRepo :: a (Maybe Text)

instance (MonadBaseControl IO m, MonadUnsafeIO m, MonadIO m, MonadThrow m) => HasGithubRepo (HandlerT App m) where
    getGithubRepo = extraGithubRepo . appExtra . settings <$> getYesod

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

