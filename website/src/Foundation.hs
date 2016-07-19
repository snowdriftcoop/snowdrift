module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.GitRev
import qualified Data.List as List
import qualified Data.Text as T
import qualified Yesod.Core.Unsafe as Unsafe

import Alerts (getAlert)
import Avatar
import qualified TestHooks

-- | The foundation datatype for your application. This can be a good place
-- to keep settings and values requiring initialization before your
-- application starts running, such as database connections. Every handler
-- will have access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appGitRev      :: GitRev
    , appAuth        :: AuthSite
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each
    -- handler function. The defaultYesodMiddleware adds the response
    -- header "Vary: Accept, Accept-Language" and performs authorization
    -- checks. Some users may also want to add the defaultCsrfMiddleware,
    -- which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in
    --      either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler
    -- module of the yesod-core package.
    --
    -- yesodMiddleware :: ToTypedContent res
    --    => HandlerT site IO res -> HandlerT site IO res
    yesodMiddleware = TestHooks.middleware

    defaultLayout widget = navbarLayout "" widget

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages
    -- when in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool


-- Create the pages for auth
instance AuthMaster App where

    postLoginRoute _ = HomeR
    postLogoutRoute = postLoginRoute

    loginHandler = do
        (loginFields, enctype) <- generateFormPost (renderDivs credentialsForm)
        selectRep $ provideRep $ navbarLayout "page/auth/login" $ do
            setTitle "Login — Snowdrift.coop"
            $(widgetFile "page/auth/login")

    createAccountHandler = do
        (loginFields, enctype) <- generateFormPost (renderDivs credentialsForm)
        selectRep $ provideRep $ navbarLayout "page/auth/create-account" $ do
            setTitle "Create Account — Snowdrift.coop"
            $(widgetFile "page/auth/create-account")

    verifyAccountHandler = do
        ((_, tokenField), enctype) <-
            runFormPost
                (renderDivs (areq textField "Token"{fsAttrs=af} Nothing))
        selectRep $ provideRep $ navbarLayout "page/auth/verify-account" $ do
            setTitle "Verify Account — Snowdrift.coop"
            $(widgetFile "page/auth/verify-account")
      where af = [("autofocus","true")]

    resetPassphraseHandler = do
        (loginFields, enctype) <- generateFormPost (renderDivs credentialsForm)
        selectRep $ provideRep $ navbarLayout "page/auth/reset-passphrase" $ do
            setTitle "Passphrase Reset — Snowdrift.coop"
            $(widgetFile "page/auth/reset-passphrase")

    sendAuthEmail _ = $logDebug . T.pack . show

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler
-- context. An example is background jobs that send email. This can also be
-- useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

navbarLayout :: Text -> Widget -> Handler Html
navbarLayout pageName widget = do
    mmsg <- getMessage
    malert <- getAlert
    maybeUser  <- maybeAuth
    avatar <- getUserAvatar (StaticR img_default_avatar_png)
                            (maybe Nothing (Just . entityVal) maybeUser)

    active <- maybe (const False) (==) <$> getCurrentRoute
    howItWorksActive <- do
        r <- getCurrentRoute
        return $ case r of
            Just HowItWorksR -> True
            _                -> False
    authActive <- do
        r <- getCurrentRoute
        return $ case r of
            Just (AuthR _)        -> True
            _                     -> False

    let navbar, footer :: Widget
        navbar = $(widgetFile "default/navbar")
        footer = $(widgetFile "default/footer")

    pc <- widgetToPageContent $ do
        $(widgetFile "default/reset")
        $(widgetFile "default/breaks")
        $(widgetFile "default/fonts")
        $(widgetFile "default/grid")
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  where
    pageClasses :: (Text, Text)
    pageClasses = ("class", classes pageName)
    classes = T.unwords
            . List.tail
            . T.splitOn "/"
