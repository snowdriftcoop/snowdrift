{-# LANGUAGE OverloadedStrings #-}
module DiscussionTest
    ( discussionSpecs
    ) where

import TestImport
import qualified Data.List as L

discussionSpecs :: Spec
discussionSpecs =
    ydescribe "discussion: rethreading" $ do
        yit "loads the discussion page" $ do
            login

            get $ DiscussWikiR "snowdrift" "about"
            statusIs 200

