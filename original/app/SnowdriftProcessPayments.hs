import Import hiding (runDB, runSDB)

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Yesod.Default.Config
import qualified Database.Persist.Sql

import Settings
import qualified Mechanism as Mech

runDB :: (PersistConfig c, MonadBaseControl IO m, MonadIO m)
      => c
      -> PersistConfigPool c
      -> PersistConfigBackend c (ResourceT (LoggingT m)) a
      -> m a
runDB dbconf poolconf sql =
    runStdoutLoggingT $
        runResourceT $ Database.Persist.Sql.runPool dbconf sql poolconf

runSDB :: (PersistConfig c, MonadBaseControl IO m, MonadIO m)
      => c
      -> PersistConfigPool c
      -> WriterT t (PersistConfigBackend c (ResourceT (LoggingT m))) b
      -> m b
runSDB dbconf poolconf = fmap fst . runDB dbconf poolconf . runWriterT

main :: IO ()
main = do
    conf <- fromArgs parseExtra
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
        Database.Persist.Sql.loadConfig >>= Database.Persist.Sql.applyEnv

    pool_conf <-
        Database.Persist.Sql.createPoolConfig (dbconf :: Settings.PersistConf)

    now <- liftIO getCurrentTime

    runSDB dbconf pool_conf $ do
        projects <- lift $ Mech.projectsToPay now
        lift $ mapM_ (Mech.payout now) projects
        Mech.rebalanceAllPledges
