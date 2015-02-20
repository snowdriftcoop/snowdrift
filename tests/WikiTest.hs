{-# LANGUAGE OverloadedStrings #-}

module WikiTest
    ( wikiSpecs
    ) where

import TestImport

import Model.Language

wikiSpecs :: Spec
wikiSpecs =
    ydescribe "wiki" $ do

        yit "Unestablished user can't create" $ [marked|
            loginAs Sue
            get $ NewWikiR snowdrift LangEn "wiki-no-dice"
            statusIs 403
        |]

        yit "creates a new page" $ [marked|
            loginAs TestUser
            newWiki snowdrift LangEn "wiki-test" "wiki test: new page"
        |]

        yit "Unestablished user can't edit" $ [marked|
            loginAs Sue
            get $ EditWikiR snowdrift LangEn "wiki-test"
            statusIs 403
        |]

        yit "edits a wiki page" $ [marked|
            loginAs TestUser
            editWiki snowdrift LangEn "wiki-test" "wiki test: edit page" "testing"
        |]
