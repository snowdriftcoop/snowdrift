module Local.Github where

import Prelude

import Data.Text (Text)

class HasGithubRepo a where
    getGithubRepo :: a (Maybe Text)
