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

            get200 $ NewWikiR "snowdrift" LangEn "testpage"

            withStatus 200 False $ request $ do
                addNonce
                setUrl $ NewWikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "preview"

            withStatus 302 False $ request $ do
                addNonce
                setUrl $ NewWikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "post"

        |]

        yit "edits a wiki page" $ [marked|

            loginAs TestUser

            get200 $ EditWikiR "snowdrift" LangEn "testpage"

{- TODO - this needs to get the last_edit_id from the rendered page and pipe it through
            withStatus 200 False $ request $ do
                addNonce
                setUrl $ WikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test after edit"
                byLabel "Comment" "testing"
                addPostParam "mode" "preview"

            withStatus 302 False $ request $ do
                addNonce
                setUrl $ WikiR "snowdrift" LangEn "testpage"
                setMethod "POST"
                byLabel "Page Content" "test after edit"
                byLabel "Comment" "testing"
                addPostParam "mode" "post"

-}
        |]
