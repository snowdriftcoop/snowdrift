module Model.Wiki.Sql where

import Import

exprEditViewedBy :: UserId -> SqlExpr (Entity WikiEdit) -> SqlExpr (Value Bool)
exprEditViewedBy user_id we = we ^. WikiEditId `in_`
    (subList_select $
     from $ \vwe -> do
     where_ (vwe ^. ViewWikiEditUser ==. val user_id)
     return (vwe ^. ViewWikiEditEdit))

exprWikiPageOnProject :: SqlExpr (Entity WikiPage) -> ProjectId -> SqlExpr (Value Bool)
exprWikiPageOnProject wp project_id = wp ^. WikiPageProject ==. val project_id
