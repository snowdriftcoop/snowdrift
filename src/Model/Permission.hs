
module Model.Permission (PermissionLevel (..), permissionLevelField) where

import Import

import qualified Data.Text as T

import Model.Permission.Internal

permissionLevelField
    :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO)
    => Field m PermissionLevel
permissionLevelField =
    (radioField' . optionsPairs) $
        map (permissionLevelLabel &&& id) [minBound ..]

permissionLevelLabel :: PermissionLevel -> Text
permissionLevelLabel = T.pack . show

