{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation

import Control.Monad.Logger (logDebugSH)
import Database.Persist.Sql (runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Yesod.Core.Unsafe as Unsafe

import AppDataTypes
import Email
import Network.Mail.Mime
import qualified TestHooks

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        fromMaybe (getApprootText guessApproot app req)
                  (appRoot (appSettings app))

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

    defaultLayout = navbarLayout ""

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

-- | A form for 'Credentials', to be used when a new passphrase is chosen
createCredentialsForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                => AForm m Credentials
createCredentialsForm = Credentials
    <$> (AuthEmail <$>
            areq textField "Email"{fsAttrs=emailAttrs}  Nothing)
    <*> (ClearPassphrase <$>
            areq
                passwordField
                "New Passphrase"{fsAttrs=ppAttrs}
                Nothing)
  where
    emailAttrs = [("autofocus",""), ("autocomplete","email")]
    ppAttrs = [("minlength","9")]

-- Create the pages for auth
instance AuthMaster App where

    postLoginRoute _ = DashboardR
    postLogoutRoute = postLoginRoute

    loginHandler = do
        (loginFields, enctype) <- generateFormPost (renderDivs credentialsForm)
        navbarLayout "page/auth/login" $ do
            setTitle "Login — Snowdrift.coop"
            $(widgetFile "page/auth/login")

    createAccountHandler = do
        (loginFields, enctype) <- generateFormPost (renderDivs createCredentialsForm)
        navbarLayout "page/auth/create-account" $ do
            setTitle "Create Account — Snowdrift.coop"
            $(widgetFile "page/auth/create-account")

    verifyAccountHandler = do
        ((_, tokenField), enctype) <-
            runFormPost
                (renderDivs (areq textField "Token"{fsAttrs=af} Nothing))
        navbarLayout "page/auth/verify-account" $ do
            setTitle "Verify Account — Snowdrift.coop"
            $(widgetFile "page/auth/verify-account")
      where af = [("autofocus","true")]

    resetPassphraseHandler =
        maybeAuth >>= maybe reset (const (redirect DashboardR))
        where
          reset = do
              (loginFields, enctype) <- generateFormPost (renderDivs createCredentialsForm)
              navbarLayout "page/auth/reset-passphrase" $ do
                  setTitle "Passphrase Reset — Snowdrift.coop"
                  $(widgetFile "page/auth/reset-passphrase")

    sendAuthEmail to msg = do
        $logDebugSH msg
        s <- appSettings <$> getYesod
        if appSendMail s
            then do r <- getUrlRenderParams
                    liftIO (renderSendMail (snowdriftAuthEmail r to msg))
            else pure ()

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
    msgs <- getMessages
    maybeUser  <- maybeAuth

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

defaultLayoutNew :: Widget -> Handler Html
defaultLayoutNew widget = do
    maybeUser <- maybeAuth
    let navbar :: Widget
        navbar = $(widgetFile "main/navbar")

    -- This should catch only main.hamlet, because _main.sass is already
    -- @import-ed in page SASS and we don't need to load it here
    pc <- widgetToPageContent $(widgetFile "main/main")
    withUrlRenderer $(hamletFile "templates/main/main-wrapper.hamlet")
  where
    footer :: Widget
    footer = $(widgetFile "main/footer")
