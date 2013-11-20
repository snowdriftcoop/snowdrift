
module Model.Permission.Internal where

import Prelude

import Database.Persist.TH

import Web.PathPieces

import Data.Text as T

import Debug.Trace

data PermissionLevel = Public | Normal | Moderated deriving (Eq, Show, Read, Ord, Enum, Bounded)

derivePersistField "PermissionLevel"

instance PathPiece PermissionLevel where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show

