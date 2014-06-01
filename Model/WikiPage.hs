module Model.WikiPage where

import Import

import Control.Monad.Trans.Resource

type PageInfo = (Entity Project, Entity WikiPage)

getPageInfo :: Text -> Text -> Handler PageInfo
getPageInfo project_handle target = runDB $ do
    project <- getBy404 $ UniqueProjectHandle project_handle
    page <- getBy404 $ UniqueWikiTarget (entityKey project) target

    return (project, page)

getCommentPageId :: (MonadLogger m, MonadResource m, MonadIO m, MonadBaseControl IO m, MonadThrow m) => CommentId -> SqlPersistT m WikiPageId
getCommentPageId comment_id = do
    [ Value page_id ] <- select $ from $ \ (c `InnerJoin` p) -> do
        on_ $ c ^. CommentDiscussion ==. p ^. WikiPageDiscussion
        where_ $ c ^. CommentId ==. val comment_id
        return $ p ^. WikiPageId

    return page_id

