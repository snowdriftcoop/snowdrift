module Model.ViewType.Internal where

import Prelude

import Database.Persist.TH (derivePersistField)

data ViewType
    = ViewComments
    | ViewEdits
    | ViewApplications
    deriving (Bounded, Enum, Eq, Show, Read)

derivePersistField "ViewType"

{- instance PathPiece Role where
    fromPathPiece s = if T.null s then Nothing else Just (read $ traceShow s $ T.unpack s)
    toPathPiece = T.pack . show
-}

