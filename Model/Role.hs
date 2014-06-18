module Model.Role
    ( Role (..)
    , getRoles
    , getRolesHandler
    , presentationRoles
    , roleLabel
    , roleAbbrev
    , roleField
    ) where

import Import

import Model.Role.Internal

getRoles :: UserId -> ProjectId -> YesodDB App [Role]
getRoles user_id project_id = fmap (map unValue) $
    select $
        from $ \r -> do
        where_ (r ^. ProjectUserRoleProject ==. val project_id &&.
                r ^. ProjectUserRoleUser ==. val user_id)
        return $ r ^. ProjectUserRoleRole

getRolesHandler :: ProjectId -> Handler [Role]
getRolesHandler project_id = maybeAuthId >>= \case
    Nothing -> return []
    Just user_id -> runDB $ getRoles user_id project_id

roleLabel :: Role -> Text
roleLabel TeamMember = "Team Member"
roleLabel Moderator  = "Moderator"
roleLabel Admin      = "Admin"

roleAbbrev :: Role -> Text
roleAbbrev TeamMember = "T"
roleAbbrev Moderator  = "M"
roleAbbrev Admin      = "A"

roleField :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO) => Field m Role
roleField = (radioField' . optionsPairs) $ map (roleLabel &&& id) [minBound ..]

presentationRoles :: [Role]
presentationRoles = [minBound..maxBound]
