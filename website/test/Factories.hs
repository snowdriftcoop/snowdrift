-- | Utilities for running tests.
--
-- Based on
-- https://github.com/thoughtbot/carnival/blob/master/test/Factories.hs
module Factories (createUser) where

import ClassyPrelude

import Model
import Database.Persist
import Database.Persist.Sql

createUser :: MonadIO m => Text -> SqlPersistT m (Entity User)
createUser ident = upsert (User ident Nothing False Nothing) []
