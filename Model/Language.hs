
module Model.Language where

import Prelude

import Model.Language.TH

import Yesod
import Database.Persist.Sql

import Data.Data

import qualified Data.Text as T

[makeLanguages| de en es fr nl pl |]

defaultLanguage :: Language
defaultLanguage = LangEn

instance RenderMessage app Language where
    renderMessage _ _ = T.pack . show

