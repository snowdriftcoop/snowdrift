
module Model.ViewType
    ( ViewType (..)
    , viewTypeLabel
    , viewTypeAbbrev
    ) where

import Import

import Model.ViewType.Internal

viewTypeLabel :: ViewType -> Text
viewTypeLabel ViewApplications = "ViewApplications"

viewTypeAbbrev :: ViewType -> Text
viewTypeAbbrev ViewApplications = "ViewApplications"

