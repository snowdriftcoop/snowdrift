{-# LANGUAGE OverloadedStrings #-}

-- | Make it possible to run SqlPersistT queries on a one-off basis.
module RunPersist where

import Control.Monad.Logger
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.PostgreSQL.Simple

runPersist :: SqlPersistT IO a -> IO a
runPersist q = do
    conn <- connectPostgreSQL "" -- Needs env vars
    back <- runStderrLoggingT (normalLogging (logSimpleConn conn))
    runSqlConn q back
  where
    logSimpleConn c = LoggingT (`openSimpleConn` c)
    normalLogging = filterLogger noDebug
    noDebug = const (> LevelDebug)
