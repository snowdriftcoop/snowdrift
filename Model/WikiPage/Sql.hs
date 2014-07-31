module Model.WikiPage.Sql where

import Import

exprEditViewedBy :: UserId -> SqlExpr (Entity WikiEdit) -> SqlExpr (Value Bool)
exprEditViewedBy user_id we = we ^. WikiEditId `in_`
    (subList_select $
     from $ \vwe -> do
     where_ (vwe ^. ViewWikiEditUser ==. val user_id)
     return (vwe ^. ViewWikiEditEdit))
