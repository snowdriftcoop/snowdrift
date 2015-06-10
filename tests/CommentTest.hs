{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module CommentTest (commentSpecs) where

import           Import (pprint)
import           TestImport hiding (get)

import           Control.Monad (when, unless)
import           Database.Esqueleto
import           Data.Foldable (forM_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Yesod.Default.Config (AppConfig (..), DefaultEnv (..))
import           Yesod.Markdown (unMarkdown)

commentSpecs :: AppConfig DefaultEnv a -> Spec
commentSpecs conf = do
    tagSpecs conf
    ticketSpecs conf

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


    -- Test that 'tags:' adds tags when a comment is edited.
    --------------------------------------------------------
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

ticketSpecs :: AppConfig DefaultEnv a -> Spec
ticketSpecs AppConfig {..} = ydescribe "ticket" $
    yit "'ticket:' syntax" [marked|
    -- Ticket number is not changed when ticket is edited.
    ------------------------------------------------------
    -- Create a ticket.
    loginAs Mary
    let ticket_line = "ticket: this is a test ticket"
    postComment (enRoute NewWikiDiscussionR "about") $
        byLabel "New Topic" $
            "Testing the 'ticket:' syntax.\n" <>
            ticket_line <> "\n" <>
            "One more line, just in case."
    (comment_id, True) <- getLatestCommentId
    let comment_route =
            render appRoot $ enRoute EditWikiCommentR "about" comment_id
    mticket1 <- testDB $ getBy $ UniqueTicket comment_id

    -- Edit it.
    let new_ticket_line = "ticket: this is a changed ticket"
    editComment comment_route new_ticket_line
    mticket2 <- testDB $ getBy $ UniqueTicket comment_id
    case (mticket1, mticket2) of
        (Just (Entity ticket_id1 _), Just (Entity ticket_id2 _)) ->
            when (ticket_id1 /= ticket_id2) $
                error "ticket id changed"
        _ -> error "ticket not found"


    -- Removing the 'ticket:' line removes the ticket from the DB.
    --------------------------------------------------------------
    editComment comment_route "no tickets here"
    mticket3 <- testDB $ getBy $ UniqueTicket comment_id
    unless (mticket3 == Nothing) $
        error "ticket not deleted"
|]
