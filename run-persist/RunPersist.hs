{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Make it possible to run SqlPersistT queries on a one-off basis. All
-- methods expect you to configure the database connection through the
-- environment: PGHOST, PGDATABASE, PGDATA, PGUSER, .pgpass, etc.
module RunPersist
        ( -- * Individual queries
          runPersist
        , runPersistKeter
        , runPersistDebug
          -- * Gobs of queries
        , runPersistPool
        , runPersistPoolDebug
        ) where

import Control.Exception (bracket)
import Control.Monad.Logger
        ( runStderrLoggingT
        , LogLevel(..)
        , LoggingT(..)
        , filterLogger
        )
import Database.Persist.Postgresql
        (SqlPersistT, openSimpleConn, withPostgresqlPool)
import Database.Persist.Sql (runSqlConn, runSqlPool, SqlBackend)
import Database.PostgreSQL.Simple
        (connectPostgreSQL, ConnectInfo(..), connect, close, Connection)
import Data.Text (Text)

import ReadConfig

-- | Connect using env vars as configuration
runPersist, runPersistDebug :: SqlPersistT IO a -> IO a
runPersist = runPersist' normalLogging (connectPostgreSQL "")
runPersistDebug = runPersist' id (connectPostgreSQL "")

-- | First try reading a config file at /opt/keter/etc/postgres.yaml, then fall
-- back to environment variables (using 'runPersist'). The argument is the
-- config group, aka the name of the app so configured.
runPersistKeter :: Text -> SqlPersistT IO a -> IO a
runPersistKeter cfgName sql = do
    mcfg <- readYamlConfig "/opt/keter/etc/postgres.yaml" cfgName
    maybe
        (runPersist sql)
        (\cfg -> runPersist' normalLogging (connect (mkconn cfg)) sql)
        mcfg
  where
    mkconn (RunPersistConfig
                connectHost
                connectPort
                connectUser
                connectPassword
                connectDatabase) = ConnectInfo{..}

runPersist'
    :: (forall a. LoggingT IO a -> LoggingT IO a)
    -> IO Connection
    -> SqlPersistT IO b
    -> IO b
runPersist' filter' getconn q =
    bracket getconn close $ \conn ->
        runSqlConn q
        =<< runStderrLoggingT (filter' (logSimpleConn conn))

logSimpleConn :: Connection -> LoggingT IO SqlBackend
logSimpleConn c = LoggingT (`openSimpleConn` c)

normalLogging :: LoggingT IO a -> LoggingT IO a
normalLogging = filterLogger (const (> LevelDebug))

runPersistPool, runPersistPoolDebug
    :: ((forall a. SqlPersistT IO a -> IO a) -> IO b) -> IO b
runPersistPool = runPersistPool' normalLogging
runPersistPoolDebug = runPersistPool' id

runPersistPool'
    :: (forall a. LoggingT IO a -> LoggingT IO a)
    -> ((forall c. SqlPersistT IO c -> IO c) -> IO b)
    -> IO b
runPersistPool' filter' cont =
    runStderrLoggingT
    $ filter'
    $ withPostgresqlPool "" 10
    $ \pool -> LoggingT
    $ \_ -> cont (`runSqlPool` pool)
