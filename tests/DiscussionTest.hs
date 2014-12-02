{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DiscussionTest
    ( discussionSpecs
    ) where

import TestImport
import qualified Data.Map as M

import Network.Wai.Test (SResponse (..))
import Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Model.Language

import Control.Monad

discussionSpecs :: Spec
discussionSpecs = do
    ydescribe "discussion" $ do
        yit "loads the discussion page" $ [marked|
            loginAs TestUser

            get200 $ WikiDiscussionR "snowdrift" LangEn "about"
        |]

        yit "posts and moves some comments" $ [marked|
            loginAs TestUser

            liftIO $ putStrLn "posting root comment"

            postComment (NewWikiDiscussionR "snowdrift" LangEn "about") $ byLabel "New Topic" "Thread 1 - root message"

            liftIO $ putStrLn "posting reply comments"

            comment_map <- fmap M.fromList $ forM [1..10] $ \ i -> do
                comment_id <- getLatestCommentId

                postComment (ReplyWikiCommentR "snowdrift" LangEn "about" comment_id) $ byLabel "Reply" $ T.pack $ "Thread 1 - reply " ++ show (i :: Integer)

                return (i, comment_id)

            let rethread_url = RethreadWikiCommentR "snowdrift" LangEn "about" $ comment_map M.! 4

            get200 rethread_url

            withStatus 302 True $ request $ do
                addNonce
                setMethod "POST"
                setUrl rethread_url
                byLabel "New Parent Url" "/p/snowdrift/w/en/about/d"
                byLabel "Reason" "testing"
                addPostParam "mode" "post"
        |]


    ydescribe "discussion - rethreading" $ do
        let createComments = [marked|
                postComment (NewWikiDiscussionR "snowdrift" LangEn "about") $ byLabel "New Topic" "First message"
                first <- getLatestCommentId
                postComment (NewWikiDiscussionR "snowdrift" LangEn "about") $ byLabel "New Topic" "Second message"
                second <- getLatestCommentId

                return (first, second)
            |]

            testRethread first second = [marked|
                let rethread_url c = RethreadWikiCommentR "snowdrift" LangEn "about" c

                get200 $ rethread_url first

                withStatus 302 True $ request $ do
                    addNonce
                    setMethod "POST"
                    setUrl $ rethread_url first
                    byLabel "New Parent Url" $ T.pack $ "/p/snowdrift/w/en/about/c/" ++ (\ (PersistInt64 i) -> show i) (unKey second)
                    byLabel "Reason" "testing"
                    addPostParam "mode" "post"

                get200 $ WikiCommentR "snowdrift" LangEn "about" second

                printBody

                bodyContains "First message"
                bodyContains "Second message"
            |]


        yit "can move newer comments under older" $ [marked|
            loginAs TestUser

            get200 $ NewWikiDiscussionR "snowdrift" LangEn "about"

            (first, second) <- createComments

            testRethread first second
        |]


        yit "can move older comments under newer" $ [marked|
            loginAs TestUser

            get200 $ NewWikiDiscussionR "snowdrift" LangEn "about"

            (first, second) <- createComments

            testRethread second first
        |]

        yit "can rethread across pages and the redirect still works" $ [marked|
            loginAs TestUser

            postComment (NewWikiDiscussionR "snowdrift" LangEn "about") $ byLabel "New Topic" "posting on about page"
            originalId <- getLatestCommentId

            get200 $ RethreadWikiCommentR "snowdrift" LangEn "about" originalId

            withStatus 302 True $ request $ do
                addNonce
                setMethod "POST"
                setUrl $ RethreadWikiCommentR "snowdrift" LangEn "about" originalId
                byLabel "New Parent Url" "/p/snowdrift/w/en/intro/d"
                byLabel "Reason" "testing cross-page rethreading"
                addPostParam "mode" "post"

            withStatus 301 True $ get $ WikiCommentR "snowdrift" LangEn "about" originalId

            Just location <- do
                statusIsResp 301
                withResponse ( \ SResponse { simpleHeaders = h } ->
                                    return $ lookup "Location" h
                             )

            newId <- getLatestCommentId
            let new_url = BSC.unpack location
                -- desired_url = "http://localhost:3000/p/snowdrift/w/intro/c/" ++ (\ (PersistInt64 i) -> show i) (unKey newId)
                desired_url = "http://localhost:3000/c/" ++ (\ (PersistInt64 i) -> show i) (unKey newId)

            assertEqual ("Redirect not matching! (" ++ show new_url ++ " /=  " ++ show desired_url ++ ")") new_url desired_url
        |]


