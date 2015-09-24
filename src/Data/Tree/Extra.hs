module Data.Tree.Extra where

import Prelude (Ordering, (.), map)

import Data.List (sortBy)
import Data.Tree (Forest, Tree(..))

sortTreeBy :: (Tree a -> Tree a -> Ordering) -> Tree a -> Tree a
sortTreeBy f (Node x xs) = Node x (sortForestBy f xs)

sortForestBy :: (Tree a -> Tree a -> Ordering) -> Forest a -> Forest a
sortForestBy f = sortBy f . map (sortTreeBy f)

singleton :: a -> Tree a
singleton x = Node x []
