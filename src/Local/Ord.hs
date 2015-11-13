module Local.Ord (prefOrdering) where

import Prelude
import Data.Function (on)

prefOrdering :: Eq b => [b] -> (a -> b) -> a -> a -> Ordering
prefOrdering prefs toPref =
    flip compare `on`
        (flip lookup (zip (reverse prefs) [1::Integer ..]) . toPref)
