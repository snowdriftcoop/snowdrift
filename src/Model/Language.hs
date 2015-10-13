{-# LANGUAGE LambdaCase #-}

module Model.Language where

import Prelude

import Data.Data
import Data.Function (on)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.Persist.Sql
import Yesod
import qualified Data.Text as T

import Model.Language.TH

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

languagePreferenceOrder :: [Language] -> (a -> Language) -> a -> a -> Ordering
languagePreferenceOrder langs getLang = flip compare `on` (flip lookup (zip (reverse langs) [1 :: Integer ..]) . getLang)

getLanguages :: MonadHandler m => m [Language]
getLanguages = cached $ nub . mapMaybe fromPathPiece <$> languages
