{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Markdown.Diff where

import Prelude
import Data.Algorithm.Diff

import Yesod.Markdown

import Database.Persist.TH
import Database.Persist.Store (PersistField (..))

import Control.Arrow

import Data.Maybe

import qualified Data.Text as T

newtype DiffInfo = DiffInfo DI deriving (Eq)

instance Read DiffInfo where
    readsPrec _ ('F':xs) = [(DiffInfo F, xs)]
    readsPrec _ ('S':xs) = [(DiffInfo S, xs)]
    readsPrec _ ('B':xs) = [(DiffInfo B, xs)]
    readsPrec _ _ = []

instance Ord DiffInfo where
    compare (DiffInfo F) (DiffInfo F) = EQ
    compare (DiffInfo F) (DiffInfo B) = GT
    compare (DiffInfo F) (DiffInfo S) = GT
    compare (DiffInfo B) (DiffInfo F) = LT
    compare (DiffInfo B) (DiffInfo B) = EQ
    compare (DiffInfo B) (DiffInfo S) = GT
    compare (DiffInfo S) (DiffInfo F) = LT
    compare (DiffInfo S) (DiffInfo B) = LT
    compare (DiffInfo S) (DiffInfo S) = EQ

instance Show DiffInfo where
    show (DiffInfo x) = show x


data MarkdownDiff = MarkdownDiff [(DiffInfo, T.Text)] deriving (Show, Read, Eq, Ord)

derivePersistField "MarkdownDiff"

diffSecond :: [(DiffInfo, a)] -> [a]
diffSecond =
    let not_first (DiffInfo B, s) = Just s
        not_first (DiffInfo S, s) = Just s
        not_first _ = Nothing
     in mapMaybe not_first

markdownDiffSecond :: MarkdownDiff -> Markdown
markdownDiffSecond (MarkdownDiff diff) = Markdown . T.concat . diffSecond $ diff

diffMarkdown :: Markdown -> Markdown -> MarkdownDiff
diffMarkdown (Markdown m1) (Markdown m2) = MarkdownDiff $ map (first DiffInfo) $ getDiff (T.lines m1) (T.lines m2) 
