{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BlogTest
    ( blogSpecs
    ) where

import TestImport
import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML

import Database.Esqueleto hiding (get)

import Network.Wai.Test (SResponse (..))
import Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BSC

import Control.Monad

import Data.Maybe (fromMaybe)

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
            login

            get $ ProjectR "snowdrift"

            statusIs 200

        {-
            htmlNoneContain "#post" "Above fold."
            htmlNoneContain "#post" "Below fold."
        -}
        |]


        yit "loads the project blog - no blog post" $ [marked|
            login

            get $ ProjectBlogR "snowdrift"

            statusIs 200

            htmlNoneContain ".post" "Above fold."
            htmlNoneContain ".post" "Below fold."
        |]


        yit "previews blog post" $ [marked|
            login

            previewBlog (NewProjectBlogPostR "snowdrift") $ do
                byLabel "Post Title" "Test"
                byLabel "Post Handle" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            bodyContains "Above fold."
            bodyContains "Below fold."
        |]


        yit "posts blog post" $ [marked|
            login

            postBlog (NewProjectBlogPostR "snowdrift") $ do
                byLabel "Post Title" "Test"
                byLabel "Post Handle" "test"
                byLabel "Content" "Above fold.\n***\nBelow fold."

            Just route <- extractLocation

            get $ decodeUtf8 route

            statusIs 200

            htmlAnyContain ".post" "Above fold."
            htmlNoneContain ".post" "Below fold."

            get $ ProjectBlogPostR "snowdrift" "test"

            htmlAnyContain ".post" "Above fold."
            htmlAnyContain ".post" "Below fold."
        |]

        yit "loads the project blog - with blog post" $ [marked|
            login

            get $ ProjectBlogR "snowdrift"

            statusIs 200

            htmlAnyContain ".post" "Above fold."
            htmlNoneContain ".post" "Below fold."
        |]

    {-
        yit "loads the project page - with blog post" $ [marked|
            login

            get $ ProjectR "snowdrift"

            statusIs 200

            htmlAllContain "#post" "Above fold."
            htmlNoneContain "#post" "Below fold."
        |]
    -}



