
module Model.Permission.Internal where

import Prelude

import Database.Persist.TH
import Database.Persist.Store

import Web.PathPieces

import Data.Text as T

import Debug.Trace

data Permission = Can't | CanView | CanEdit deriving (Eq, Show, Read, Ord, Enum)

derivePersistField "Permission"

instance PathPiece Permission where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show

