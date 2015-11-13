module Local.Esqueleto
        (module Upstream, on_, valList, key, entitiesMap, onEntity)
        where

import Prelude

import Database.Esqueleto as Upstream hiding (on, valList)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Database.Esqueleto as Upstream
import qualified Data.Map as M

on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = Upstream.on

-- Like Database.Esqueleto.valList, but more generic.
valList :: (Esqueleto query expr backend, PersistField typ, Foldable l) => l typ -> expr (ValueList typ)
valList = Upstream.valList . toList

-- XXX: Will this always succeed?
key :: PersistEntity record => PersistValue -> Key record
key v = let Right k = keyFromValues [v] in k

entitiesMap :: Ord (Key t) => [Entity t] -> Map (Key t) t
entitiesMap = foldr (\(Entity k v) -> M.insert k v) mempty

-- | Convenience function for unwrapping an Entity and supplying both the key and value to another function.
onEntity :: (Key a -> a -> b) -> Entity a -> b
onEntity f (Entity x y) = f x y
