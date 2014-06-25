module Model.Project.Internal where

import Data.Function (on)
import Prelude       (Ord, compare)

import Model

-- Somewhat arbitrary Ord instance, but convenient for some views. Order by name.
instance Ord Project where
    compare = compare `on` projectName
