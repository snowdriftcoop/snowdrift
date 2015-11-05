-- | Define the Count typeclass and a Int64 instance for nice autoderiving.
module Model.Count where

import Prelude

import Data.Int

class Count a where
    getCount :: a -> Int64

instance Count Int64 where getCount = id
