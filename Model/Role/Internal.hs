
module Model.Role.Internal where

import Prelude

import Database.Persist.TH

import Web.PathPieces

import Data.Text as T

import Debug.Trace

data Role
    = TeamMember
    | Moderator
    | Admin
    deriving (Bounded, Enum, Eq, Show, Read, Ord)

derivePersistField "Role"

instance PathPiece Role where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show
