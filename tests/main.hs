{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import TestImport
import Yesod.Default.Config
-- import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)

import DiscussionTest
import WikiTest

import System.IO

main :: IO ()
main = do
    liftIO $ hPutStrLn stderr "starting test program" >> hFlush stderr
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }

    liftIO $ hPutStrLn stderr "building foundation" >> hFlush stderr
    foundation <- makeFoundation conf


    liftIO $ hPutStrLn stderr "running test" >> hFlush stderr

    hspec $ do
        yesodSpec foundation $ do
            wikiSpecs

            discussionSpecs

