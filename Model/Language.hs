
module Model.Language where

import Prelude

import Model.Language.TH

import Yesod
import Database.Persist.Sql

import Data.Data

import Control.Arrow
import Data.List

import qualified Data.Text as T

[makeLanguages| de en es fr nl pl |]

defaultLanguage :: Language
defaultLanguage = LangEn

instance RenderMessage app Language where
    renderMessage _ _ = T.pack . show

makeLanguageOptions :: [Language] -> [(T.Text, Language)]
makeLanguageOptions preferred_languages = map (toPathPiece &&& id) $ preferred_languages ++ ([minBound..maxBound] \\ preferred_languages)
