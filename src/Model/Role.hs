module Model.Role
    ( Role (..)
    , presentationRoles
    , roleLabel
    , roleAbbrev
    , roleField
    ) where

import Import

roleLabel :: Role -> Text
roleLabel TeamMember = "Team Member"
roleLabel Moderator  = "Moderator"
roleLabel Admin      = "Admin"

roleAbbrev :: Role -> Text
roleAbbrev TeamMember = "T"
roleAbbrev Moderator  = "M"
roleAbbrev Admin      = "A"

roleField
    :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO)
    => Field m Role
roleField = (radioField' . optionsPairs) $ map (roleLabel &&& id) [minBound ..]

presentationRoles :: [Role]
presentationRoles = [minBound..maxBound]
