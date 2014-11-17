{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BlogTest
    ( blogSpecs
    ) where

import TestImport
import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML

import Data.Text.Encoding

blogSpecs :: Spec
blogSpecs = do
    let postBlog route stmts = [marked|
            get route
            statusIs 200

            [ form ] <- htmlQuery "form"

            let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS

            request $ do
                addNonce
                setMethod "POST"
                let route' = maybe (Left route) Right $ M.lookup "action" $ getAttrs form
                either setUrl setUrl route'
                addPostParam "mode" "post"
                stmts

            statusIsResp 302
        |]

        previewBlog route stmts = [marked|
            get route
            statusIs 200

            [ form ] <- htmlQuery "form"

            let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS

            request $ do
                addNonce
                setMethod "POST"
                maybe (setUrl route) setUrl $ M.lookup "action" $ getAttrs form

                addPostParam "mode" "preview"
                stmts

            statusIs 200
        |]



    ydescribe "blog" $ do

        yit "loads the project page - no blog post" $ [marked|
            loginAs TestUser

            get $ ProjectR "snowdrift"

            statusIs 200

        {-
            htmlNoneContain "#blog-post" "Above fold."
            htmlNoneContain "#blog-post" "Below fold."
        -}
        |]


        yit "loads the project blog - no blog post" $ [marked|
            loginAs TestUser

            get $ ProjectBlogR "snowdrift"

            statusIs 200

            htmlNoneContain ".blog-post" "Above fold."
            htmlNoneContain ".blog-post" "Below fold."
        |]


        yit "previews blog post" $ [marked|
            loginAs AdminUser

            previewBlog (NewProjectBlogPostR "snowdrift") $ do
                byLabel "Title for this blog post" "Test"
                byLabel "Handle for the URL" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            bodyContains "Above fold."
            bodyContains "Below fold."
        |]


        yit "posts blog post" $ [marked|
            loginAs AdminUser

            postBlog (NewProjectBlogPostR "snowdrift") $ do
                byLabel "Title for this blog post" "Test"
                byLabel "Handle for the URL" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            Just route <- extractLocation

            get $ decodeUtf8 route

            statusIs 200

            htmlAnyContain ".blog-post-top" "Above fold."
            htmlNoneContain ".blog-post-top" "Below fold."

            get $ ProjectBlogPostR "snowdrift" "test"

            htmlAnyContain ".blog-post" "Above fold."
            htmlAnyContain ".blog-post" "Below fold."
        |]

        yit "loads the project blog - with blog post" $ [marked|
            loginAs TestUser

            get $ ProjectBlogR "snowdrift"

            statusIs 200

            htmlAnyContain ".blog-post-top" "Above fold."
            htmlNoneContain ".blog-post-top" "Below fold."
        |]

    {-
        yit "loads the project page - with blog post" $ [marked|
            loginAs TestUser

            get $ ProjectR "snowdrift"

            statusIs 200

            htmlAllContain "#blog-post" "Above fold."
            htmlNoneContain "#blog-post" "Below fold."
        |]
    -}



