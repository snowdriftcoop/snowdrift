module Model.Blog
  ( fetchProjectBlogPostDB
  , concatContent
  , splitContent
  , postBlogPostDB
  , updateBlogPostDB
  ) where

import Import

import Control.Monad.Writer.Strict (tell)
import qualified Data.Text as T

import View.Project

fetchProjectBlogPostDB :: Text -> Text -> YDB (Entity Project, Entity BlogPost)
fetchProjectBlogPostDB project_handle blog_post_handle = do
    p@(Entity project_id _) <- getBy404 $ UniqueProjectHandle project_handle
    b <- getBy404 $ UniqueBlogPost project_id blog_post_handle
    return (p, b)

concatContent :: Markdown -> Maybe Markdown -> Markdown
concatContent top_content mbottom_content =
    Markdown $ make (unMarkdown top_content) (unMarkdown <$> mbottom_content)
  where
    make top (Just bottom) = top <> "***" <> "\n" <> bottom
    make top _ = top

splitContent :: Markdown -> (Markdown, Maybe Markdown)
splitContent (Markdown text) = case break (== "***") (T.lines text) of
    -- TODO: reject empty content above the fold?
    (top, []) -> (Markdown $ T.unlines top, Nothing)
    (top, ["***"]) -> (Markdown $ T.unlines top, Nothing)
    (top, "***":bottom) ->
        (Markdown $ T.unlines top, Just $ Markdown $ T.unlines bottom)
    (_, _) -> error "cannot result from a break"

postBlogPostDB
    :: Text
    -> Text
    -> UserId
    -> ProjectId
    -> Markdown
    -> SDB BlogPostId
postBlogPostDB title handle user_id project_id content = do
    let (top_content, bottom_content) = splitContent content

    now <- liftIO getCurrentTime

    (post_id, post) <- lift $ do
        discussion_id <- insert $ Discussion 0

        let doInsert counter = do
                let modifier = maybe
                        ""
                        (T.cons '~' . T.pack . show)
                        (counter :: Maybe Integer)
                    next_counter = Just $ maybe 2 (+1) counter

                    post = BlogPost
                        now
                        title
                        (handle <> modifier)
                        user_id
                        project_id
                        discussion_id
                        top_content
                        bottom_content

                maybe_post_id <- insertUnique post

                case maybe_post_id of
                    Just post_id -> return (post_id, post)
                    _ -> doInsert next_counter

        doInsert Nothing

    tell [EBlogPost post_id post]

    return post_id

updateBlogPostDB :: UserId -> BlogPostId -> ProjectBlog -> DB ()
updateBlogPostDB user_id blog_post_id ProjectBlog {..} = do
    let (top_content, bottom_content) = splitContent projectBlogContent
    update $ \bp -> do
        set bp [ BlogPostUser          =. val user_id
               , BlogPostTitle         =. val projectBlogTitle
               , BlogPostHandle        =. val projectBlogHandle
               , BlogPostTopContent    =. val top_content
               , BlogPostBottomContent =. val bottom_content
               ]
        where_ $ bp ^. BlogPostId ==. val blog_post_id
