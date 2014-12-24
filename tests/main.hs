{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import TestImport
import Yesod.Default.Config
-- import Yesod.Test
import Test.Hspec (hspec, describe, it)
import Application (makeFoundation)

import UserTest
import NotifyTest
import EmailTest
import DiscussionTest
import WikiTest
import BlogTest

import TestHandler
import Model.Markdown

import Control.Exception (bracket)
import Data.Maybe (fromJust)
import System.Directory (removeFile, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.IO
import System.IO.Unsafe

main :: IO ()
main = do
    liftIO $ hPutStrLn stderr "starting test program" >> hFlush stderr
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }

    liftIO $ hPutStrLn stderr "building foundation" >> hFlush stderr
    foundation <- makeFoundation conf


    liftIO $ hPutStrLn stderr "running test" >> hFlush stderr

    -- We have to use an environment variable because 'hspec' does not
    -- allow to use command line options.
    options <- maybe [] words <$> lookupEnv "SNOWDRIFT_TESTING_OPTIONS"
    if "test-email-daemon" `elem` options
        then withTempFile $ spec foundation
        else spec foundation Nothing

withTempFile :: (Maybe FilePath -> IO a) -> IO ()
withTempFile f = bracket
    (do tmp <- getTemporaryDirectory; openTempFile tmp "emails")
    (removeFile . fst)
    (\(file, handle) -> do hClose handle; void $ f $ Just file)

spec :: App -> Maybe FilePath -> IO ()
spec foundation mfile =
    hspec $ do
        describe "fix links" $ do
            it "works correctly on all examples" $ do
                let mismatches = unsafePerformIO $ testHandler testFixLinks
                case mismatches of
                    Right [] -> True
                    _ -> False

        let app_config = settings foundation
        yesodSpec foundation $ do
            userSpecs
            notifySpecs app_config
            if isJust mfile
                then emailSpecs app_config $ fromJust mfile
                else return ()
            wikiSpecs
            blogSpecs
            discussionSpecs
