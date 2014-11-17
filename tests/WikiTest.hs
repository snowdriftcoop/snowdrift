{-# LANGUAGE OverloadedStrings #-}
module WikiTest
    ( wikiSpecs
    ) where

import TestImport

import Model.Language

wikiSpecs :: Spec
wikiSpecs =
    ydescribe "wiki" $ do

        yit "creates a new page" $ [marked|

            loginAs TestUser

            get $ NewWikiR "snowdrift" LangEn "testpage"
            statusIs 200

            request $ do
                addNonce
                setUrl $ NewWikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "preview"

            statusIs 200

            request $ do
                addNonce
                setUrl $ NewWikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "post"

            statusIs 302

        |]

        yit "edits a wiki page" $ [marked|

            loginAs TestUser

            get $ EditWikiR "snowdrift" LangEn "testpage"
            statusIs 200

{- TODO - this needs to get the last_edit_id from the rendered page and pipe it through
            request $ do
                addNonce
                setUrl $ WikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test after edit"
                byLabel "Comment" "testing"
                addPostParam "mode" "preview"

            statusIs 200

            request $ do
                addNonce
                setUrl $ WikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test after edit"
                byLabel "Comment" "testing"
                addPostParam "mode" "post"

            statusIs 302
-}

        |]
