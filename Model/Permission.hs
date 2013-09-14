
module Model.Permission (PermissionLevel (..), permissionLevelField) where

import Import

import Model.Permission.Internal

import qualified Data.Text as T

permissionLevelField :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO) => Field m PermissionLevel
permissionLevelField = radioFieldList $ map (permissionLevelLabel &&& id) [minBound ..]

permissionLevelLabel :: PermissionLevel -> Text
permissionLevelLabel = T.pack . show
