{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module TagTest (tagSpecs) where

import           TestImport hiding (get, editComment)

import           Control.Monad (when)
import           Database.Esqueleto
import           Data.Foldable (forM_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Yesod.Default.Config (AppConfig (..), DefaultEnv (..))
import           Yesod.Markdown (unMarkdown)

tagSpecs :: AppConfig DefaultEnv a -> Spec
tagSpecs AppConfig {..} = ydescribe "tags" $
    yit "'tags:' syntax" [marked|
    -- Test that 'tags:' adds tags when a comment is created.
    ---------------------------------------------------------
    -- Add a few tags.
    loginAs user
    let tags_line = tagsLine tags_post_comment
    postComment (enRoute NewWikiDiscussionR "about") $
        byLabel "New Topic" $
            "Testing the 'tags:' syntax.\n" <>
             tags_line <> "\n" <>
            "One more line, just in case."
    (comment_id, True) <- getLatestCommentId
    let comment_route =
            render appRoot $ enRoute EditWikiCommentR "about" comment_id

    -- Check that the added tags are present in the database.
    errorUnlessUniqueTags comment_id tags_post_comment

    -- Check that the "tags:" line is stripped from the contents.
    comment_text <- getCommentText comment_id
    errorWhenInfixOf tags_line comment_text


    -- Test that 'tags:' adds tags when a comment is edited".
    ---------------------------------------------------------
    -- Add more tags.
    editComment comment_route $
        comment_text <> "\n" <>
        tagsLine tags_edit_comment

    -- Check that new tags are added.
    errorUnlessUniqueTags comment_id tags_edit_comment

    -- Check that old tags are not removed.
    errorUnlessUniqueTags comment_id tags_post_comment

    -- Check that the "tags:" line is stripped from the contents.
    getCommentText comment_id >>= errorWhenInfixOf tags_line


    -- Test that editing a comment does not remove tags.
    ----------------------------------------------------
    -- Edit a comment without changing anything.
    editComment comment_route comment_text

    -- Check that the tags are still there.
    errorUnlessUniqueTags comment_id $ tags_post_comment <> tags_edit_comment
    |]
  where
    user = Mary

    foo  = "foo"
    bar  = "bar"
    baz  = "baz"
    qux  = "qux"
    quux = "quux"

    tagsLine tags = "tags: " <> (Text.intercalate ", " tags)

    tags_post_comment = [foo, bar, baz]
    -- Duplicates are added intentionally, so the tests could check
    -- that they are removed.
    tags_edit_comment = [bar, qux, foo, quux, quux]

    errorUnlessUniqueTags comment_id tags = do
        user_id <- userId user
        forM_ tags $ \tag_name ->
            testDB $ errorUnlessUniqueTag comment_id user_id tag_name

    errorWhenInfixOf subt t =
        when (subt `Text.isInfixOf` t) $
            error $ Text.unpack $ subt <> " appears in " <> t

    getCommentText comment_id = fmap unMarkdown $ testDB $
        get comment_id >>=
            maybe (error $ "comment " <> pprint comment_id <> " not found")
                  (return . commentText)

class Show a => PPrint a where
    pprint :: a -> String

instance PPrint CommentId where
    pprint = show . unSqlBackendKey . unCommentKey

instance PPrint UserId where
    pprint = show . unSqlBackendKey . unUserKey

instance PPrint TagId where
    pprint = show . unSqlBackendKey . unTagKey

instance PPrint Text where
    pprint = Text.unpack

-- XXX: Merge with 'editComment' from 'TestImport'.
editComment :: Text -> Text -> YesodExample App ()
editComment route comment_text = [marked|
    get200 route

    withStatus 303 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        addPostParam "f1" comment_text
        addPostParam "f2" "en"
        addPostParam "mode" "post"
    |]

errorUnlessUniqueTag :: CommentId -> UserId -> Text -> SqlPersistM ()
errorUnlessUniqueTag comment_id user_id tag_name = do
    mtag_id <- fmap (fmap entityKey) $ getBy $ UniqueTag tag_name
    maybe (error $ "tag name " <> pprint tag_name <> " not found")
          (\tag_id -> do
                mcomment_tag <- getBy $ UniqueCommentTag comment_id tag_id user_id
                maybe (error $ "comment tag with"
                            <> " comment id "   <> pprint comment_id
                            <> ", tag id "      <> pprint tag_id
                            <> ", and user id " <> pprint user_id
                            <> " not found")
                      (\comment_tag ->
                           when (commentTagCount (entityVal comment_tag) /= 1) $
                               error $ "tag name "
                                    <> pprint tag_name
                                    <> " not unique")
                      mcomment_tag)
          mtag_id
