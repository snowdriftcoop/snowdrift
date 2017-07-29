{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Data.Aeson (Result(..), fromJSON, withObject, (.!=), (.:?))
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Text.Shakespeare.Sass (genBuildId, wfsSass)
import Web.Stripe (StripeKey(..))
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
        (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

import Discourse (DiscourseSecret(..))

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    , appSendMail :: Bool
    -- ^ Whether to send emails
    , appStripeSecretKey        :: StripeKey
    , appStripePublishableKey   :: StripeKey
    , appDiscourseSsoSecret     :: DiscourseSecret
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let runningDevelopment =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .:? "static-dir" .!= "static"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= runningDevelopment
        appShouldLogAll           <- o .:? "should-log-all"   .!= runningDevelopment
        appReloadTemplates        <- o .:? "reload-templates" .!= runningDevelopment
        appMutableStatic          <- o .:? "mutable-static"   .!= runningDevelopment
        appSkipCombining          <- o .:? "skip-combining"   .!= runningDevelopment
        appSendMail               <- o .:? "send-email"       .!= not runningDevelopment
        appStripePublishableKey   <- StripeKey . encodeUtf8 <$> o .: "stripe-publishable-key"
        appStripeSecretKey        <- StripeKey . encodeUtf8 <$> o .: "stripe-secret-key"
        appDiscourseSsoSecret     <- (DiscourseSecret . encodeUtf8) <$> o .: "discourse-sso-secret"

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = wfsSass $genBuildId ["templates"]

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
-- Using impureThrow since that mimics existing behavior (which used to be
-- Control.Exception.throw). I don't care to fix this, since I would rather
-- rewrite this entire module.
configSettingsYmlValue :: Value
configSettingsYmlValue = either impureThrow id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
