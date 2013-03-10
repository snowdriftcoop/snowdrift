
module Model.Role.Internal where

import Prelude

import Database.Persist.TH
import Database.Persist.Store

import Web.PathPieces

import Data.Text as T

import Debug.Trace

data Role = Uninvited | GeneralPublic | TeamMember | CommitteeCandidate | CommitteeMember | BoardMember | Editor | Admin deriving (Eq, Show, Read, Ord, Enum)

derivePersistField "Role"

instance PathPiece Role where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show

