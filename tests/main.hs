{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import TestImport

import Control.Exception (bracket)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO
import System.IO.Unsafe
import Test.Hspec (hspec, describe, it)
import Yesod.Default.Config
import qualified Data.Text as Text


import Application (makeFoundation)
import Model.Markdown
import TestHandler

-- All test modules
import BlogTest
import CommentTest
import DiscussionTest
import NotifyTest
import UserTest
import WikiTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    withTempFile $ spec foundation

withTempFile :: (FileName -> IO a) -> IO ()
withTempFile f = bracket
    (do tmp <- getTemporaryDirectory; openTempFile tmp "emails")
    (removeFile . fst)
    (\(file, handle) -> do hClose handle; void $ f $ FileName $ Text.pack file)

spec :: App -> FileName -> IO ()
spec foundation file =
    hspec $ do
        describe "fix links" $
            it "works correctly on all examples" $ do
                let mismatches = unsafePerformIO $ testHandler testFixLinks
                case mismatches of
                    Right [] -> True
                    _ -> False

        yesodSpec foundation $ do
            let config = appSettings foundation
            userSpecs
            notifySpecs config file
            wikiSpecs
            blogSpecs
            discussionSpecs
            commentSpecs config
