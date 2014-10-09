module Model.Blog (postBlogPostDB) where

import Import

import           Control.Monad.Writer.Strict          (tell)

import           Yesod.Markdown

import qualified Data.Text as T


postBlogPostDB :: Text -> Text -> UserId -> ProjectId -> Markdown -> SDB BlogPostId
postBlogPostDB title handle user_id project_id content = do
    let (top_content, bottom_content) = transform content

    now <- liftIO getCurrentTime

    (post_id, post) <- lift $ do
        discussion_id <- insert $ Discussion 0

        let doInsert counter = do
                let modifier = maybe "" (T.cons '~' . T.pack . show) (counter :: Maybe Integer)
                    next_counter = Just $ maybe 2 (+1) counter

                    post = BlogPost now title (handle <> modifier) user_id project_id discussion_id top_content bottom_content

                maybe_post_id <- insertUnique post

                case maybe_post_id of
                    Just post_id -> return (post_id, post)
                    _ -> doInsert next_counter

        doInsert Nothing

    tell [EBlogPost post_id post]

    return post_id

  where
    transform (Markdown text) = case break (== "***") (T.lines text) of
        -- TODO: reject empty content above the fold?
        (top, []) -> (Markdown $ T.unlines top, Nothing)
        (top, ["***"]) -> (Markdown $ T.unlines top, Nothing)
        (top, "***":bottom) -> (Markdown $ T.unlines top, Just $ Markdown $ T.unlines bottom)
        (_, _) -> error "cannot result from a break"

