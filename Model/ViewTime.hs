module Model.ViewTime
    ( getCommentsViewTime
    , updateCommentsViewTime
    ) where

import Import

import Model.ViewType (ViewType(..))

getCommentsViewTime :: UserId -> ProjectId -> YesodDB App (Maybe (Entity ViewTime))
getCommentsViewTime user_id project_id = getBy $ UniqueViewTimeUserProjectType user_id project_id ViewComments

updateCommentsViewTime :: UTCTime -> UserId -> ProjectId -> YesodDB App ()
updateCommentsViewTime now user_id project_id = do
    c <- updateCount $ \vt -> do
         set vt [ ViewTimeTime =. val now ]
         where_ (vt ^. ViewTimeUser    ==. val user_id &&.
                 vt ^. ViewTimeProject ==. val project_id &&.
                 vt ^. ViewTimeType    ==. val ViewComments)
    when (c == 0) $
      insert_ $ ViewTime user_id project_id ViewComments now
