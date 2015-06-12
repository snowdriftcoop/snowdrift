{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module CommentTest (commentSpecs) where

import           Import (pprint)
import           TestImport hiding (get)

import           Control.Monad (when, unless)
import           Database.Esqueleto
import           Data.Foldable (forM_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Yesod (RedirectUrl, Route)
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
ticketSpecs conf = ydescribe "ticket" $
    yit "'ticket:' syntax" $ do
        testTicket conf Mary
            (enRoute NewWikiDiscussionR "about")
            (enRoute ReplyWikiCommentR "about")
            (enRoute EditWikiCommentR "about")

        mary_id <- userId Mary
        testTicket conf Mary
            (NewUserDiscussionR mary_id)
            (ReplyUserCommentR mary_id)
            (EditUserCommentR mary_id)

        testTicket conf Mary
            (NewProjectDiscussionR "snowdrift")
            (ReplyProjectCommentR "snowdrift")
            (EditProjectCommentR "snowdrift")

        -- Depends on the blog test from 'NotifyTest'.
        testTicket conf Mary
            (NewBlogPostDiscussionR "snowdrift" "testing")
            (ReplyBlogPostCommentR "snowdrift" "testing")
            (EditBlogPostCommentR "snowdrift" "testing")

testTicket
    :: (RedirectUrl App url1, RedirectUrl App url2, Login user)
    => AppConfig DefaultEnv a
    -> user -> url1 -> (CommentId -> url2) -> (CommentId -> Route App)
    -> Example ()
testTicket AppConfig {..} user new_route reply_route edit_route = [marked|
    -- Ticket number is not changed when ticket is edited.
    ------------------------------------------------------
    -- Create a ticket.
    loginAs user
    let new_ticket_line = "ticket: this is a new ticket"
    postComment new_route $
        byLabel "New Topic" $
            "Testing the 'ticket:' syntax.\n" <>
            new_ticket_line <> "\n" <>
            "One more line, just in case."
    (new_comment_id, True) <- getLatestCommentId
    mnew_ticket <- testDB $ getBy $ UniqueTicket new_comment_id
    when (mnew_ticket == Nothing) $ error "new ticket not found"

    -- Reply to it.
    let reply_ticket_line = "ticket: this is a replied ticket"
    postComment (reply_route new_comment_id) $
        byLabel "Reply" $ "Replying\n" <> reply_ticket_line
    (reply_comment_id, True) <- getLatestCommentId
    mreply_ticket <- testDB $ getBy $ UniqueTicket reply_comment_id
    let comment_route = render appRoot $ edit_route reply_comment_id

    -- Edit the reply.
    let edit_ticket_line = "ticket: this is an edited ticket"
    editComment comment_route edit_ticket_line
    medit_ticket <- testDB $ getBy $ UniqueTicket reply_comment_id
    case (mreply_ticket, medit_ticket) of
        (Just (Entity reply_ticket_id _), Just (Entity edit_ticket_id _)) ->
            when (reply_ticket_id /= edit_ticket_id) $ error "ticket id changed"
        _ -> error "ticket not found"


    -- Removing the 'ticket:' line removes the ticket from the DB.
    --------------------------------------------------------------
    editComment comment_route "no tickets here"
    medit_ticket2 <- testDB $ getBy $ UniqueTicket reply_comment_id
    unless (medit_ticket2 == Nothing) $ error "ticket not deleted"
|]
