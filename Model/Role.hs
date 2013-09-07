
module Model.Role 
    ( Role (..)
    , roleLabel
    , roleAbbrev
    , roleField
    ) where

import Import

import Model.Role.Internal


roleLabel :: Role -> Text
roleLabel TeamMember = "Team Member"
roleLabel Moderator = "Moderator"
roleLabel Admin = "Admin"

roleAbbrev :: Role -> Text
roleAbbrev TeamMember = "T"
roleAbbrev Moderator = "M"
roleAbbrev Admin = "A"

roleField :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO) => Field m Role
roleField = radioFieldList $ map (roleLabel &&& id) [minBound ..]

