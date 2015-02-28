import Import hiding (runDB)

import Settings
import Yesod.Default.Config

import qualified Data.Text as T

import Model.Project
import Model.Currency

import qualified Database.Persist.Sql

import Control.Monad.Logger
import Control.Monad.Trans.Resource

import Data.Typeable

import Control.Exception.Lifted (throw, catch, Exception)

retry :: Monad m => m Bool -> m ()
retry x = x >>= \ x' -> unless x' $ retry x

data NegativeBalances = NegativeBalances ProjectId [UserId] deriving (Show, Typeable)

instance Exception NegativeBalances

main :: IO ()
main = do
    conf <- fromArgs parseExtra
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
        Database.Persist.Sql.loadConfig >>= Database.Persist.Sql.applyEnv

    pool_conf <- Database.Persist.Sql.createPoolConfig (dbconf :: Settings.PersistConf)

    now <- liftIO getCurrentTime

    let runDB :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => SqlPersistT m a -> m a
        runDB sql = Database.Persist.Sql.runPool dbconf sql pool_conf

        payout (Entity project_id project, Entity payday_id _) = runDB $ do
            let project_name = projectName project

            liftIO $ putStrLn $ T.unpack project_name

            pledges <- select $ from $ \ pledge -> do
                where_ $ pledge ^. PledgeProject ==. val project_id
                    &&. pledge ^. PledgeFundedShares >. val 0

                return pledge

            user_balances <- forM pledges $ \ (Entity _ pledge) -> do
                Just user <- get $ pledgeUser pledge
                let amount = projectShareValue project $* fromIntegral (pledgeFundedShares pledge)
                    user_account_id = userAccount user
                    project_account_id = projectAccount project

                void $ insert $ Transaction now (Just project_account_id) (Just user_account_id) (Just payday_id) amount "Project Payout" Nothing

                user_account <- updateGet user_account_id [AccountBalance Database.Persist.Sql.-=. amount]
                _            <- updateGet project_account_id [AccountBalance Database.Persist.Sql.+=. amount]

                return (pledgeUser pledge, accountBalance user_account)

            let negative_balances = filter ((< 0) . snd) user_balances

            when (not $ null negative_balances) $ throw $ NegativeBalances project_id $ map fst negative_balances

            update $ \ p -> do
                set p [ ProjectLastPayday =. val (Just payday_id) ]
                where_ $ p ^. ProjectId ==. val project_id

            return True


        dropPledges (NegativeBalances project_id negative_balances) = runDB $ do
            update $ \ p -> do
                set p [ PledgeFundedShares -=. val 1 ]
                where_ $ p ^. PledgeUser `in_` valList negative_balances
                    &&. p ^. PledgeFundedShares >. val 0

            updateShareValue project_id

            return False

    runStdoutLoggingT $ runResourceT $ do
        projects <- runDB $ select $ from $ \ (project `LeftOuterJoin` last_payday `InnerJoin` payday) -> do
            on_ $ payday ^. PaydayDate >. coalesceDefault [ last_payday ?. PaydayDate ] (project ^. ProjectCreatedTs)
            on_ $ project ^. ProjectLastPayday ==. last_payday ?. PaydayId

            where_ $ payday ^. PaydayDate <=. val now

            orderBy [ asc $ payday ^. PaydayDate, desc $ project ^. ProjectShareValue ]

            return (project, payday)

        forM_ projects $ retry . flip catch dropPledges . payout


