-- | This module has some tools for testing Esqueleto queries from ghci.
--
-- It is not intended to be used in any Snowdrift code. Instead, :load it
-- from `cabal repl`.
module DBTest (dbdev) where

import Import
import Yesod.Default.Config
import qualified Database.Persist
import Settings
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

-- | Query type
--
-- Generally, query types will need to be specified. This type is
-- a shortcut for writing them down.
--
-- It can probably be improved upon. Maybe more specific types for
-- different types of queries?
type Q a = SqlPersistT (ResourceT IO) a

-- | Synonym for Q within this module
--
-- Q is a bit too brief for actual code, and shouldn't be confused with TH
-- stuff.
type Query a = Q a

-- | Run an esqueleto query in ghci.
--
-- Two examples:
--
-- >>> dbdev ((select $ from $ \p -> return (p ^. UserIdent)) :: Q [Value Text])
-- [Value "admin",Value "anonymous"]
--
-- >>> fmap (map entityVal) $ dbdev (select $ from $ return :: Q [Entity User])
-- [User {userIdent = "admin", userEmail = ...
dbdev :: Show a => Query a -> IO a
dbdev = dbtest Development

-- | Small generalization of dbdev over the different execution environments.
--
-- May be useful later.
dbtest :: Show a => DefaultEnv -> Query a -> IO a
dbtest env query = do
    conf <- Yesod.Default.Config.loadConfig (configSettings env)
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    pool <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    runResourceT $ Database.Persist.runPool dbconf query pool
