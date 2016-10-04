{-# LANGUAGE OverloadedStrings #-}

-- | Make it possible to run SqlPersistT queries on a one-off basis.
module RunPersist where

import Control.Monad.Logger
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.PostgreSQL.Simple

runPersist q = do
    conn <- connectPostgreSQL "dbname='snowdrift_development'" -- Needs env vars
    back <- runStderrLoggingT (logSimpleConn conn)
    runSqlConn q back
  where
    logSimpleConn c = LoggingT (`openSimpleConn` c)
