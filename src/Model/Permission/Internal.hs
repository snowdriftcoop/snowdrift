
module Model.Permission.Internal where

import Prelude

import Data.Text as T
import Database.Persist.TH
import Debug.Trace
import Web.PathPieces

data PermissionLevel = Public | Normal | Moderated
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

derivePersistField "PermissionLevel"

instance PathPiece PermissionLevel where
    fromPathPiece s =
        if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show

