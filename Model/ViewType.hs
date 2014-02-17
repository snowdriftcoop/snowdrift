
module Model.ViewType 
    ( ViewType (..)
    , viewTypeLabel
    , viewTypeAbbrev
    ) where

import Import

import Model.ViewType.Internal


viewTypeLabel :: ViewType -> Text
viewTypeLabel ViewComments = "ViewComments"
viewTypeLabel ViewEdits = "ViewEdits"
viewTypeLabel ViewApplications = "ViewApplications"

viewTypeAbbrev :: ViewType -> Text
viewTypeAbbrev ViewComments = "ViewComments"
viewTypeAbbrev ViewEdits = "ViewEdits"
viewTypeAbbrev ViewApplications = "ViewApplications"
