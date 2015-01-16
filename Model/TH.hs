{-# LANGUAGE RecordWildCards #-}

module Model.TH where

import Language.Haskell.TH
import Database.Persist.Types

import Prelude

import Control.Monad
import Data.Monoid
import Data.Char

import qualified Data.Text as T
import qualified Data.List as L

mkReferences :: String -> [EntityDef] -> Q [Dec]
mkReferences name defs = do
    let references = do
            EntityDef {..} <- defs
            FieldDef {..} <- entityFields
            guard $ fieldType == FTTypeCon Nothing (T.pack name <> "Id")
            return $ map (ucHead . T.unpack . unHaskellName) [entityHaskell, fieldHaskell]

        ucHead [] = []
        ucHead (c:cs) = toUpper c : cs

        mkTypeConstructor names = NormalC (mkName $ L.intercalate "_" $ name <> "Ref" : names) []

    return [ DataD [] (mkName $ name <> "Reference") [] (map mkTypeConstructor references) [mkName "Bounded", mkName "Enum"] ]

