import Import

import Settings
import Application
import Yesod.Default.Config

import Database.Persist.Sql (loadConfig, applyEnv, createPoolConfig, runPool, transactionUndo)

import qualified Data.Text as T

import Model
import Model.Project
import Model.Currency

import qualified Database.Persist.Sql

import Control.Monad.Logger
import Control.Monad.Trans.Resource

import System.Log.FastLogger

import qualified Data.ByteString as BS

import Data.Time

import Blaze.ByteString.Builder (toByteString)

retry x = x >>= \ x' -> when (not x') $ retry x

main :: IO ()
main = do
    conf <- fromArgs parseExtra
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
        Database.Persist.Sql.loadConfig >>= Database.Persist.Sql.applyEnv

    p <- Database.Persist.Sql.createPoolConfig (dbconf :: Settings.PersistConf)

    let runDB :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => SqlPersistT m a -> m a
        runDB sql = Database.Persist.Sql.runPool dbconf sql p

    runStdoutLoggingT $ runResourceT $ do
        projects <- runDB $ select $ from $ return

        forM_ projects $ \ (Entity project_id project) -> runDB $ do
            let project_name = projectName project

            liftIO $ putStrLn $ T.unpack project_name

            now <- liftIO getCurrentTime

            pledges <- select $ from $ \ pledge -> do
                where_ $ pledge ^. PledgeProject ==. val project_id
                return pledge

            user_balances <- forM pledges $ \ (Entity pledge_id pledge) -> do
                Just user <- get $ pledgeUser pledge
                let amount = projectShareValue project $* fromIntegral (pledgeFundedShares pledge)
                    user_account_id = userAccount user
                    project_account_id = projectAccount project

                insert $ Transaction now (Just project_account_id) (Just user_account_id) amount "Project Payout" Nothing

                user_account <- updateGet user_account_id [AccountBalance Database.Persist.Sql.-=. amount]
                project_account <- updateGet project_account_id [AccountBalance Database.Persist.Sql.+=. amount]

                return (pledgeUser pledge, accountBalance user_account)

            let negative_balances = filter ((< 0) . snd) user_balances


            if null negative_balances
             then transactionSave >> return True
             else do
                transactionUndo

                update $ \ p -> do
                    set p [ PledgeFundedShares -=. val 1 ]
                    where_ $ p ^. PledgeUser `in_` valList (map fst negative_balances)

                updateShareValue project_id

                transactionSave

                return False

        return ()

