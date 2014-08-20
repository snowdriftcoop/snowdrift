{-# LANGUAGE OverloadedStrings #-}
module WikiTest
    ( wikiSpecs
    ) where

import TestImport

wikiSpecs :: Spec
wikiSpecs =
    ydescribe "wiki" $ do

        yit "creates a new page" $ [marked|

            login

            get $ NewWikiR "snowdrift" "testpage"
            statusIs 200

            request $ do
                addNonce
                setUrl $ NewWikiR "snowdrift" "testpage"
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "preview"

            statusIs 200

            request $ do
                addNonce
                setMethod "POST"
                byLabel "Page Content" "test"
                addPostParam "mode" "post"
        |]

