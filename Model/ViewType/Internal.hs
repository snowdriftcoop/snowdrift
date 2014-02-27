
module Model.ViewType.Internal where

import Prelude

import Database.Persist.TH

import Web.PathPieces

import Data.Text as T

import Debug.Trace

import Data.Universe

data ViewType = ViewComments | ViewEdits | ViewApplications deriving (Bounded, Enum, Eq, Show, Read)

instance Universe ViewType where
    universe = [ViewComments .. ViewApplications]

derivePersistField "ViewType"

{- instance PathPiece Role where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show
-}

