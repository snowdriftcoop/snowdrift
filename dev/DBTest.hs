module DBTest (dbdev) where

import Import
import Yesod.Default.Config
import qualified Database.Persist
import Settings
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

type Query a =
    SqlPersistT
        (ResourceT IO)
        a

-- | Run an esqueleto query in ghci. Two examples:
--
-- >>> dbdev ((select $ from $ \p -> return (p ^. UserIdent)) :: Query [Value Text])
-- [Value "admin",Value "anonymous"]
--
-- >>> fmap (map entityVal) $ dbdev (select $ from $ return :: Query [Entity User])
-- [User {userIdent = "admin", userEmail = ...
--
dbdev :: Show a => Query a -> IO a
dbdev = dbtest Development

dbtest :: (Show a, Show env) => env -> Query a -> IO a
dbtest env query = do
    conf <- Yesod.Default.Config.loadConfig (configSettings env)
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    pool <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    runResourceT $ Database.Persist.runPool dbconf query pool
