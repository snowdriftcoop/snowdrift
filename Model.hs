{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Yesod.Auth.HashDB (HashDBUser (..))
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

import Data.Time.Clock (UTCTime)
import Data.Int (Int64)

import Model.Currency (Milray)
import Model.Role.Internal (Role)
import Model.Permission.Internal (PermissionLevel)
import Model.Markdown.Diff (MarkdownDiff)

import Yesod.Markdown (Markdown)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = userHash
    userPasswordSalt = userSalt
    setSaltAndPasswordHash salt hash user = user { userHash = Just hash, userSalt = Just salt }

