module Model.ViewTime
    ( getCommentViewTimes
    ) where

import Import

import Model.ViewType (ViewType(..))

getCommentViewTimes :: UserId -> ProjectId -> YesodDB App [ViewTime]
getCommentViewTimes user_id project_id = fmap (map entityVal) $
    select $
        from $ \vt -> do
        where_ (vt ^. ViewTimeUser    ==. val user_id &&.
                vt ^. ViewTimeProject ==. val project_id &&.
                vt ^. ViewTimeType    ==. val ViewComments)
        return vt
