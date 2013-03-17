import Import

import Settings
import Application
import Yesod.Default.Config

import Database.Persist.Store
import Database.Persist.GenericSql

import qualified Data.Text as T

import Model
import Model.Project
import Model.Currency

import Data.Time

main = do
    conf <- fromArgs parseExtra
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv

    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)

    let runDB :: SqlPersist IO a -> IO a
        runDB sql = Database.Persist.Store.runPool dbconf sql p

    projects <- runDB $ selectList [] [] -- TODO restrict

    forM_ projects $ \ (Entity project_id project) -> runDB $ do
        let project_name = projectName project
        liftIO $ putStrLn $ T.unpack project_name
        
        now <- liftIO getCurrentTime

        pledges <- selectList [PledgeProject ==. project_id] []

        forM_ pledges $ \ (Entity pledge_id pledge) -> do
            Just user <- get $ pledgeUser pledge
            let amount = projectShareValue project $* fromIntegral (pledgeFundedShares pledge)
                user_account_id = userAccount user
                project_account_id = projectAccount project

            insert $ Transaction now (Just project_account_id) (Just user_account_id) amount "Project Payout" Nothing

            user_account <- updateGet user_account_id [AccountBalance -=. amount]
            project_account <- updateGet project_account_id [AccountBalance +=. amount]

            when (accountBalance user_account < 0) rollback

