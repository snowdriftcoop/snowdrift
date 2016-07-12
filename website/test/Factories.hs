{-# LANGUAGE RecordWildCards #-}
-- | Utilities for running tests.
--
-- Based on
-- https://github.com/thoughtbot/carnival/blob/master/test/Factories.hs
module Factories (createUser) where

import ClassyPrelude

import Database.Persist.Sql (SqlPersistT)

import AuthSite

createUser :: MonadIO m => Text -> Text -> SqlPersistT m ()
createUser e p = do
    ProvisionalUser{..} <-
        liftIO
            (provisional (Credentials ee pp) (Verification ee (AuthToken "stuff")))
    privilegedCreateUser
        (VerifiedUser provisionalUserEmail provisionalUserDigest)
  where
    ee = AuthEmail e
    pp = ClearPassphrase p
