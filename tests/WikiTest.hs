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
            newWiki snowdrift LangEn "wiki-test" "wiki test: new page"
        |]

        yit "edits a wiki page" $ [marked|
            loginAs TestUser
            editWiki snowdrift LangEn "wiki-test" "wiki test: edit page" "testing"
        |]
