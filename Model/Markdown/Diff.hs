{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Markdown.Diff where

import Prelude
import Data.Algorithm.Diff

import Yesod.Markdown

import Database.Persist.TH

import Data.Maybe

import qualified Data.Text as T

data DiffInfo = S | B | F deriving (Eq, Ord, Read, Show)


data MarkdownDiff = MarkdownDiff [(DiffInfo, T.Text)] deriving (Show, Read, Eq, Ord)

derivePersistField "MarkdownDiff"

diffSecond :: [(DiffInfo, a)] -> [a]
diffSecond =
    let not_first (B, s) = Just s
        not_first (S, s) = Just s
        not_first _ = Nothing
     in mapMaybe not_first

markdownDiffSecond :: MarkdownDiff -> Markdown
markdownDiffSecond (MarkdownDiff diff) = Markdown . T.concat . diffSecond $ diff

diffToDiffInfo :: Diff a -> (DiffInfo, a)
diffToDiffInfo (First x) = (F, x)
diffToDiffInfo (Both _ x) = (B, x)
diffToDiffInfo (Second x) = (S, x)

diffMarkdown :: Markdown -> Markdown -> MarkdownDiff
diffMarkdown (Markdown m1) (Markdown m2) = MarkdownDiff $ map diffToDiffInfo $ getDiff (T.lines m1) (T.lines m2) 
