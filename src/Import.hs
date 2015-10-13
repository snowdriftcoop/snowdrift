{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies #-}

module Import (module Import) where

import Control.Arrow as Import
            ((***), (&&&), (+++), first, second, (>>>), (<<<))
import Control.Monad as Import
import Data.Foldable as Import (toList)
import Data.Function as Import (on)
import Data.Int as Import (Int64)
import Data.Map as Import (Map)
import Data.Maybe as Import
            (fromMaybe, listToMaybe, mapMaybe, isJust, catMaybes)
import Data.Monoid as Import ((<>))
import Data.Set as Import (Set)
import Data.Text as Import (Text)
import Data.Time.Clock as Import (UTCTime, diffUTCTime, getCurrentTime)
import Database.Esqueleto as Import hiding (on, valList)
import Prelude as Import hiding (head, init, last, readFile, tail, writeFile)
import Yesod as Import
            hiding (Route (..)
                   ,(||.)
                   ,(==.)
                   ,(!=.)
                   ,(<.)
                   ,(<=.)
                   ,(>.)
                   ,(>=.)
                   ,(=.)
                   ,(+=.)
                   ,(-=.)
                   ,(*=.)
                   ,(/=.)
                   ,selectSource
                   ,delete
                   ,update
                   ,count
                   ,Value
                   ,runDB
                   ,languages)
import Yesod.Auth as Import
import Yesod.Form.Bootstrap3 as Import
import Yesod.Markdown as Import (Markdown (..))
import qualified Database.Esqueleto
import qualified Data.Map as M

import Foundation as Import
import Model as Import
import Model.Language as Import
import Model.Comment.Internal as Import hiding (TagName, TicketName)
import Model.Established.Internal as Import
import Model.Role.Internal as Import
import Model.SnowdriftEvent.Internal as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import

on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = Database.Esqueleto.on

-- Like Database.Esqueleto.valList, but more generic.
valList :: (Esqueleto query expr backend, PersistField typ, Foldable l) => l typ -> expr (ValueList typ)
valList = Database.Esqueleto.valList . toList

-- XXX: Will this always succeed?
key :: PersistEntity record => PersistValue -> Key record
key v = let Right k = keyFromValues [v] in k

entitiesMap :: Ord (Key t) => [Entity t] -> Map (Key t) t
entitiesMap = foldr (\(Entity k v) -> M.insert k v) mempty

-- | Convenience function for unwrapping an Entity and supplying both the key and value to another function.
onEntity :: (Key a -> a -> b) -> Entity a -> b
onEntity f (Entity x y) = f x y
