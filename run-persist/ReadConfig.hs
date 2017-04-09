{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Gather connection info from a config file, rather than the environment.
-- This expects the same sort of config file that keter does. An example looks
-- like:
-- @@
-- ConfigOption:
--   user: usernamestuff
--   name: databasenamestuff
--   server: localhost
--   port: 5432
--   pass: passwordlol
-- @@
module ReadConfig (readYamlConfig, RunPersistConfig(..)) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Char
import Data.Text (Text)
import Data.Yaml
import GHC.Generics hiding (from)

-- | All config necessary for connecting to a Postgres instance.
data RunPersistConfig = RunPersistConfig
        { runPersistUser :: Text
        , runPersistName :: Text
        , runPersistServer :: Text
        , runPersistPort :: Int
        , runPersistPass :: Text
        }
        deriving (Show, Generic)

instance ToJSON RunPersistConfig where
    toEncoding = genericToEncoding defaultOptions
                    { fieldLabelModifier = (_head %~ toLower) . drop 10 }

instance FromJSON RunPersistConfig where
    parseJSON = genericParseJSON defaultOptions
                    { fieldLabelModifier = (_head %~ toLower) . drop 10 }

-- | Given a config file and a named config, find it
readYamlConfig :: FilePath -> Text -> IO (Maybe RunPersistConfig)
readYamlConfig yml configName = runMaybeT $ do
    obj :: Value <- MaybeT $ decodeFile yml
    thing <- MaybeT $ pure $ obj ^? key configName
    MaybeT $ pure $ thing ^? _JSON
