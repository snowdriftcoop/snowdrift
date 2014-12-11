{-# LANGUAGE LambdaCase #-}

module Model.Language where

import Prelude

import Model.Language.TH

import Yesod
import Database.Persist.Sql

import Data.Data

import qualified Data.Text as T
import           Data.Text (Text)

[makeLanguages| de en es fr nl pl pt |]

defaultLanguage :: Language
defaultLanguage = LangEn

instance RenderMessage app Language where
    renderMessage _ _ = T.pack . show

renderLanguage :: Language -> Language -> Text
renderLanguage LangEn = \case
    LangDe -> "German"
    LangEn -> "English"
    LangEs -> "Spanish"
    LangFr -> "French"
    LangNl -> "Dutch"
    LangPl -> "Polish"
    LangPt -> "Portuguese"

renderLanguage _ = renderLanguage LangEn

