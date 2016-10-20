{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Make it possible to run SqlPersistT queries on a one-off basis. All
-- methods expect you to configure the database connection through the
-- environment: PGHOST, PGDATABASE, PGDATA, PGUSER, .pgpass, etc.
module RunPersist
        ( -- * Individual queries
          runPersist
        , runPersistDebug
          -- * Gobs of queries
        , runPersistPool
        , runPersistPoolDebug
        ) where

import Control.Monad.Logger
        ( runStderrLoggingT
        , LogLevel(..)
        , LoggingT(..)
        , filterLogger
        )
import Database.Persist.Postgresql (SqlPersistT, openSimpleConn, withPostgresqlPool)
import Database.Persist.Sql (runSqlConn, runSqlPool)
import Database.PostgreSQL.Simple (connectPostgreSQL)

runPersist, runPersistDebug :: SqlPersistT IO a -> IO a
runPersist = runPersist' normalLogging
runPersistDebug = runPersist' id

runPersist'
    :: (forall a. LoggingT IO a -> LoggingT IO a)
    -> SqlPersistT IO b
    -> IO b
runPersist' filter' q = do
    conn <- connectPostgreSQL "" -- Needs env vars
    back <- runStderrLoggingT (filter' (logSimpleConn conn))
    runSqlConn q back
  where
    logSimpleConn c = LoggingT (`openSimpleConn` c)

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
    $ \logfunc -> cont (`runSqlPool` pool)
