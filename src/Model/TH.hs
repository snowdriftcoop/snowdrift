{-# LANGUAGE RecordWildCards #-}

module Model.TH where

import Prelude

import Control.Monad
import Data.Char
import Data.Monoid
import Database.Persist.Types
import Language.Haskell.TH
import qualified Data.List as L
import qualified Data.Text as T

mkReferences :: String -> [EntityDef] -> Q [Dec]
mkReferences name defs = do
    let references = do
            EntityDef {..} <- defs
            FieldDef {..} <- entityFields
            guard $ fieldType == FTTypeCon Nothing (T.pack name <> "Id")
            return $
                map (ucHead . T.unpack . unHaskellName)
                    [entityHaskell, fieldHaskell]

        ucHead [] = []
        ucHead (c:cs) = toUpper c : cs

        mkTypeConstructor names =
            NormalC (mkName $ L.intercalate "_" $ name <> "Ref" : names) []

    return
        [ DataD
            []
            (mkName $ name <> "Reference")
            []
            (map
                mkTypeConstructor
                references)
            [mkName "Bounded", mkName "Enum"]]
