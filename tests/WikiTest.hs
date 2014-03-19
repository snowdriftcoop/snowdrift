{-# LANGUAGE OverloadedStrings #-}
module WikiTest
    ( wikiSpecs
    ) where

import TestImport
import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML

import Database.Esqueleto hiding (get)

import Data.Text as T

import Control.Monad

wikiSpecs :: Spec
wikiSpecs =
    ydescribe "wiki" $ do
        yit "creates a new page" $ do
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


