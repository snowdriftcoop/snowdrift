{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BlogTest
    ( blogSpecs
    ) where

import TestImport

import Data.Text.Encoding
import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML

blogSpecs :: Spec
blogSpecs = do
    let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS
        blogRoute = NewBlogPostR "snowdrift"
    ydescribe "blog" $ do
        yit "previews blog post" $ [marked|
            loginAs AdminUser

            get200 $ ProjectBlogR "snowdrift"
            htmlNoneContain ".blog-post" "Above fold."
            htmlNoneContain ".blog-post" "Below fold."

            get200 blogRoute

            [ form ] <- htmlQuery "form"

            withStatus 200 False $ request $ do
                addToken
                setMethod "POST"
                maybe (setUrl blogRoute) setUrl $ M.lookup "action" $ getAttrs form

                addPostParam "mode" "preview"
                byLabel "Title for this blog post" "Test"
                byLabel "Handle for the URL" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            bodyContains "Above fold."
            bodyContains "Below fold."
        |]

        yit "posts blog post" $ [marked|
            loginAs AdminUser

            get200 $ ProjectBlogR "snowdrift"
            htmlNoneContain ".blog-post" "Above fold."
            htmlNoneContain ".blog-post" "Below fold."

            get200 blogRoute

            [ form ] <- htmlQuery "form"

            withStatus 303 True $ request $ do
                addToken
                setMethod "POST"
                let route' = maybe (Left blogRoute) Right $ M.lookup "action" $ getAttrs form
                either setUrl setUrl route'
                addPostParam "mode" "post"
                byLabel "Title for this blog post" "Test"
                byLabel "Handle for the URL" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            Just route <- extractLocation

            get200 $ decodeUtf8 route
            htmlAnyContain ".blog-post-top" "Above fold."
            htmlNoneContain ".blog-post-top" "Below fold."

            get $ BlogPostR "snowdrift" "test"
            htmlAnyContain ".blog-post" "Above fold."
            htmlAnyContain ".blog-post" "Below fold."

            get200 $ ProjectBlogR "snowdrift"
            htmlAnyContain ".blog-post-top" "Above fold."
            htmlNoneContain ".blog-post-top" "Below fold."
        |]
