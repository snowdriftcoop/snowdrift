{-# LANGUAGE RecordWildCards #-}

module Model.Discussion.TH where

import Language.Haskell.TH
import Database.Persist.Types

import Prelude

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Map as M

mkDiscussionTypes :: [EntityDef] -> Q [Dec]
mkDiscussionTypes defs = do
    let discussion_types = flip mapMaybe defs $ \ EntityDef {..} -> do
            guard $ entityHaskell /= HaskellName "Comment"
            guard $ any (\ FieldDef {..} -> fieldType == FTTypeCon Nothing "DiscussionId") entityFields
            return $ T.unpack $ unHaskellName entityHaskell

        mkTypeConstructor name = NormalC (mkName $ "DiscussionType" <> name) []

        mkOnConstructors names = case runState (mapM mkOnConstructor names) (M.fromList [ ("WikiPage", "WikiTarget") ]) of
            (constructors, missed)
                | M.null missed -> constructors
                | otherwise -> error $ "unmatched exceptions in building DiscussionOn payloads: " ++ show (M.keys missed)

        mkOnConstructor name = do
            entity_name <- grabEntityName name

            return $ NormalC (mkName $ "DiscussionOn" <> name) [(NotStrict, AppT (ConT $ mkName "Entity") (ConT $ mkName entity_name))]

        grabEntityName name = state $ \ exceptions -> case M.lookup name exceptions of
            Just name' -> (name', M.delete name exceptions)
            Nothing -> (name, exceptions)
            

    return
        [ DataD [] (mkName "DiscussionType") [] (map mkTypeConstructor discussion_types) [mkName "Bounded", mkName "Enum"]
        , DataD [] (mkName "DiscussionOn") [] (mkOnConstructors discussion_types) []
        ]

