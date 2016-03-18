{-# LANGUAGE RecordWildCards #-}

module Model.Discussion.TH where

import Prelude

import Control.Monad
import Data.Maybe
import Data.Monoid
import Database.Persist.Types
import Language.Haskell.TH
import qualified Data.Text as T

mkDiscussionTypes :: [EntityDef] -> Q [Dec]
mkDiscussionTypes defs = do
    let discussion_types = flip mapMaybe defs $ \EntityDef {..} -> do
            guard $ entityHaskell /= HaskellName "Comment"
            guard
                (any
                    (\FieldDef {..} ->
                        fieldType == FTTypeCon Nothing "DiscussionId")
                    entityFields)
            return $ T.unpack $ unHaskellName entityHaskell

        mkTypeConstructor name = NormalC (mkName $ "DiscussionType" <> name) []

    return
        [ DataD
            []
            (mkName "DiscussionType")
            []
            (map mkTypeConstructor discussion_types)
            [mkName "Bounded", mkName "Enum"]
        ]
