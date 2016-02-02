{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DiscussionTest
    ( discussionSpecs
    ) where

import TestImport
import Import (key)

import Control.Monad
import Network.Wai.Test (SResponse (..))
import Yesod (RedirectUrl)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as M
import qualified Data.Text as T

import Model.Language

discussionSpecs :: Spec
discussionSpecs = do
    forM_ [minBound..maxBound] $ \case
        DiscussionTypeWikiPage ->
            runDiscussionTest
                "wiki page"
                (WikiDiscussionR "snowdrift" LangEn "about")
                (WikiCommentR "snowdrift" LangEn "about")
                (NewWikiDiscussionR "snowdrift" LangEn "about")
                (ReplyWikiCommentR "snowdrift" LangEn "about")
                (RethreadWikiCommentR "snowdrift" LangEn "about")

        DiscussionTypeBlogPost ->
            runDiscussionTest
                "blog post"
                (BlogPostDiscussionR "snowdrift" "test")
                (BlogPostCommentR "snowdrift" "test")
                (NewBlogPostDiscussionR "snowdrift" "test")
                (ReplyBlogPostCommentR "snowdrift" "test")
                (RethreadBlogPostCommentR "snowdrift" "test")

        DiscussionTypeProject ->
            runDiscussionTest
                "project"
                (ProjectDiscussionR "snowdrift")
                (ProjectCommentR "snowdrift")
                (NewProjectDiscussionR "snowdrift")
                (ReplyProjectCommentR "snowdrift")
                (RethreadProjectCommentR "snowdrift")

        DiscussionTypeUser ->
            runDiscussionTest
                "user"
                (UserDiscussionR user_id)
                (UserCommentR user_id)
                (NewUserDiscussionR user_id)
                (ReplyUserCommentR user_id)
                (RethreadUserCommentR user_id)
            where user_id = key $ PersistInt64 1

runDiscussionTest :: (Show url, RedirectUrl App url)
                  => String
                  -> url
                  -> (CommentId -> url)
                  -> url
                  -> (CommentId -> url)
                  -> (CommentId -> url)
                  -> Spec
runDiscussionTest
        label
        discussion_page_url
        comment_url
        new_thread_url
        comment_reply_url
        comment_rethread_url = do
    ydescribe (unwords ["discussion on", label]) $ do
        yit "loads the discussion page" $ do
            loginAs TestUser
            get200 discussion_page_url

        let postReply i = do
                (comment_id, approved) <- getLatestCommentId
                when (not approved)
                     (error $ "comment not approved: " ++ show comment_id)
                postComment
                    (comment_reply_url comment_id)
                    (byLabel
                        "Reply"
                        (T.pack ("Thread 1 - reply " ++ show (i :: Integer))))
                return (i, comment_id)

        yit "posts and moves some comments" $ do
            loginAs TestUser
            postComment new_thread_url
                        (byLabel "New Topic" "Thread 1 - root message")
            comment_map <- fmap M.fromList $ forM [1..10] postReply
            let reply_comment = comment_map M.! 4
            get200 $ comment_rethread_url reply_comment
            withStatus 303 True $ request $ do
                addToken
                setMethod "POST"
                setUrl $ comment_rethread_url reply_comment
                byLabel "New Parent Url" "/p/snowdrift/w/en/about/d"
                byLabel "Reason" "testing"
                addPostParam "mode" "post"


    ydescribe (unwords ["discussion on", label, "- rethreading"]) $ do
        let createComments = do
                postComment new_thread_url $ byLabel "New Topic" "First message"
                (first_message, True) <- getLatestCommentId
                postComment new_thread_url $ byLabel "New Topic" "Second message"
                (second_message, True) <- getLatestCommentId

                return (first_message, second_message)

            testRethread first_message second_message = do

                get200 $ comment_rethread_url first_message

                withStatus 303 True $ request $ do
                    addToken
                    setMethod "POST"
                    setUrl $ comment_rethread_url first_message
                    byLabel
                        "New Parent Url"
                        (T.pack
                            ("/p/snowdrift/w/en/about/c/"
                                ++ (\(PersistInt64 i) -> show i)
                                    (toPersistValue second_message)))
                    byLabel "Reason" "testing"
                    addPostParam "mode" "post"

                get200 $ comment_url second_message

                bodyContains "First message"
                bodyContains "Second message"


        yit "can move newer comments under older" $ do
            loginAs TestUser
            (first_message, second_message) <- createComments
            testRethread first_message second_message


        yit "can move older comments under newer" $ do
            loginAs TestUser
            (first_message, second_message) <- createComments
            testRethread second_message first_message

        yit "can rethread across pages and the redirect still works" $ do
            loginAs TestUser
            postComment new_thread_url $ byLabel "New Topic" "posting on about page"
            (originalId, True) <- getLatestCommentId
            get200 $ comment_rethread_url originalId

            withStatus 303 True $ request $ do
                addToken
                setMethod "POST"
                setUrl $ comment_rethread_url originalId
                byLabel "New Parent Url" "/p/snowdrift/w/en/intro/d"
                byLabel "Reason" "testing cross-page rethreading"
                addPostParam "mode" "post"

            withStatus 301 True $ get $ comment_url originalId

            Just location <- do
                statusIsResp 301
                withResponse ( \SResponse { simpleHeaders = h } ->
                                    return $ lookup "Location" h
                             )

            (newId, True) <- getLatestCommentId
            let new_url = BSC.unpack location
                desired_url =
                    "http://localhost:3000/c/"
                        ++ (\(PersistInt64 i) -> show i) (toPersistValue newId)

            assertEqual
                ("Redirect not matching! ("
                    ++ show new_url
                    ++ " /=  "
                    ++ show desired_url
                    ++ ")")
                new_url
                desired_url

