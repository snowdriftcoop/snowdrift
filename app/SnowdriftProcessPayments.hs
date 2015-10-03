import Import hiding (runDB, runSDB)

import Control.Exception.Lifted (throw, Exception)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Typeable
import Yesod.Default.Config
import qualified Data.Text as T
import qualified Database.Persist.Sql

import Model.Currency
import Model.Project
import Settings

data NegativeBalances = NegativeBalances ProjectId [UserId]
    deriving (Show, Typeable)

instance Exception NegativeBalances

payout :: (MonadIO m, Functor m)
       => UTCTime
       -> (Entity Project, Entity Payday)
       -> SqlPersistT m Bool
payout now (Entity project_id project, Entity payday_id _) = do
    let project_name = projectName project

    pledges <- select $ from $ \pledge -> do
        where_ $ pledge ^. PledgeProject ==. val project_id
            &&. pledge ^. PledgeFundedShares >. val 0

        return pledge

    user_balances <- forM pledges $ \(Entity _ pledge) -> do
        Just user <- get $ pledgeUser pledge
        let amount =
                projectShareValue project
                $* fromIntegral (pledgeFundedShares pledge)
            user_account_id = userAccount user
            project_account_id = projectAccount project

        void $
            insert $
                Transaction now
                            (Just project_account_id)
                            (Just user_account_id)
                            (Just payday_id)
                            amount
                            "Project Payout"
                            Nothing

        user_account <-
            updateGet
                user_account_id
                [AccountBalance Database.Persist.Sql.-=. amount]
        _            <-
            updateGet
                project_account_id
                [AccountBalance Database.Persist.Sql.+=. amount]

        return (pledgeUser pledge, accountBalance user_account)

    let negative_balances = filter ((< 0) . snd) user_balances

    unless (null negative_balances)
           (throw $ NegativeBalances project_id $ map fst negative_balances)

    update $ \p -> do
        set p [ ProjectLastPayday =. val (Just payday_id) ]
        where_ $ p ^. ProjectId ==. val project_id

    liftIO $ putStrLn $ "paid to " <> T.unpack project_name

    return True

projectsToPay :: MonadIO m
              => UTCTime
              -> SqlPersistT m [(Entity Project, Entity Payday)]
projectsToPay now =
    select $
    from $ \(project
        `LeftOuterJoin` last_payday
        `InnerJoin` payday) -> do
    on_ $ payday ^. PaydayDate
        >. coalesceDefault
            [ last_payday ?. PaydayDate ]
            (project ^. ProjectCreatedTs)
    on_ $ project ^. ProjectLastPayday ==. last_payday ?. PaydayId
    where_ $ payday ^. PaydayDate <=. val now
    orderBy [ asc $ payday ^. PaydayDate
            , desc $ project ^. ProjectShareValue ]
    return (project, payday)

rebalanceAllPledges :: (MonadWriter [PledgeId] (t (ReaderT SqlBackend m))
                       ,MonadTrans t
                       ,MonadBaseControl IO m
                       ,MonadLogger m
                       ,MonadResource m
                       )
                    => t (SqlPersistT m) ()
rebalanceAllPledges = do
    unders <- lift underfundedPatrons
    unless (null unders) $ do
        maxUnders <- lift $ maxShares Nothing unders
        lift $ dropShares maxUnders
        lift $ mapM_ updateShareValue =<< updatedProjects maxUnders
        tell maxUnders
        rebalanceAllPledges

updatedProjects :: (MonadIO m, Functor m)
                => [PledgeId]
                -> SqlPersistT m [ProjectId]
updatedProjects pledges = fmap (map (pledgeProject . entityVal))
                               (selectList [PledgeId <-. pledges] [])

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
        projects <- lift $ projectsToPay now
        lift $ mapM_ (payout now) projects
        rebalanceAllPledges
