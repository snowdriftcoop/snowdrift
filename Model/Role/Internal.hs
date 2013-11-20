
module Model.Role.Internal where

import Prelude

import Database.Persist.TH

import Web.PathPieces

import Data.Text as T

import Debug.Trace

data Role = Moderator | TeamMember | Admin deriving (Bounded, Enum, Eq, Show, Read)

derivePersistField "Role"

instance PathPiece Role where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show

