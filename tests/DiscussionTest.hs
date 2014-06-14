{-# LANGUAGE OverloadedStrings #-}
module DiscussionTest
    ( discussionSpecs
    ) where

import TestImport
import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML

import Database.Esqueleto hiding (get)

import Network.Wai.Test (SResponse (..))
import Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Control.Monad

discussionSpecs :: Spec
discussionSpecs = do
    let postComment route stmts = do
            get route
            statusIs 200

            [ form ] <- htmlQuery "form"

            let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS
                Just action = M.lookup "action" $ getAttrs form

            request $ do
                addNonce
                setMethod "POST"
                setUrl action
                addPostParam "mode" "post"
                stmts

            statusIsResp 302

        getLatestCommentId = do
            [ Value (Just comment_id) ] <- runDB $ select $ from $ \ comment -> return (max_ $ comment ^. CommentId)
            return comment_id

    ydescribe "discussion" $ do
        yit "loads the discussion page" $ do
            login

            get $ DiscussWikiR "snowdrift" "about"
            statusIs 200

        yit "posts and moves some comments" $ do
            login

            liftIO $ putStrLn "posting root comment"

            postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "Thread 1 - root message"

            liftIO $ putStrLn "posting reply comments"

            comment_map <- fmap M.fromList $ forM [1..10] $ \ i -> do
                comment_id <- getLatestCommentId

                postComment (ReplyCommentR "snowdrift" "about" comment_id) $ byLabel "Reply" $ T.pack $ "Thread 1 - reply " ++ show i

                return (i, comment_id)

            let rethread_url = RethreadWikiCommentR "snowdrift" "about" $ comment_map M.! 4

            get rethread_url

            statusIs 200

            request $ do
                addNonce
                setMethod "POST"
                setUrl rethread_url
                byLabel "New Parent Url" "/p/snowdrift/w/about/d"
                byLabel "Reason" "testing"
                addPostParam "mode" "rethread"

            statusIsResp 302


    ydescribe "discussion - rethreading" $ do
        let createComments = do
                postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "First message"
                first <- getLatestCommentId
                postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "Second message"
                second <- getLatestCommentId

                return (first, second)

            testRethread first second = do
                let rethread_url c = RethreadWikiCommentR "snowdrift" "about" c

                get $ rethread_url first
                statusIs 200

                request $ do
                    addNonce
                    setMethod "POST"
                    setUrl $ rethread_url first
                    byLabel "New Parent Url" $ T.pack $ "/p/snowdrift/w/about/c/" ++ (\ (PersistInt64 i) -> show i) (unKey second)
                    byLabel "Reason" "testing"
                    addPostParam "mode" "rethread"

                statusIsResp 302

                get $ DiscussCommentR "snowdrift" "about" second
                statusIs 200

                printBody

                bodyContains "First message"
                bodyContains "Second message"


        yit "can move newer comments under older" $ do
            login

            get $ NewDiscussWikiR "snowdrift" "about"
            statusIs 200

            (first, second) <- createComments

            testRethread first second


        yit "can move older comments under newer" $ do
            login

            get $ NewDiscussWikiR "snowdrift" "about"
            statusIs 200

            (first, second) <- createComments

            testRethread second first

        yit "can rethread across pages and the redirect still works" $ do
            login

            postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "posting on about page"
            originalId <- getLatestCommentId

            get $ RethreadWikiCommentR "snowdrift" "about" originalId
            statusIs 200

            request $ do
                addNonce
                setMethod "POST"
                setUrl $ RethreadWikiCommentR "snowdrift" "about" originalId
                byLabel "New Parent Url" "/p/snowdrift/w/intro/d"
                byLabel "Reason" "testing cross-page rethreading"
                addPostParam "mode" "rethread"

            statusIsResp 302

            get $ DiscussCommentR "snowdrift" "about" originalId
            statusIsResp 301

            Just location <- do
                statusIsResp 301
                withResponse ( \ SResponse { simpleHeaders = h } ->
                                    return $ lookup "Location" h
                             )

            newId <- getLatestCommentId
            let new_url = BSC.unpack location
                desired_url = "http://localhost:3000/p/snowdrift/w/intro/c/" ++ (\ (PersistInt64 i) -> show i) (unKey newId)

            assertEqual ("Redirect not matching! (" ++ show new_url ++ " /=  " ++ show desired_url ++ ")") new_url desired_url 
                

