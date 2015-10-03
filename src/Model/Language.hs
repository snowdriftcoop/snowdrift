{-# LANGUAGE LambdaCase #-}

module Model.Language where

import Prelude

import Data.Data
import Data.Text (Text)
import Database.Persist.Sql
import Model.Language.TH
import Yesod
import qualified Data.Text as T

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

