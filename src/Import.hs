{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies #-}

module Import (module Import) where

import Control.Arrow as Import
            ((***), (&&&), (+++), first, second, (>>>), (<<<))
import Control.Monad as Import
import Data.Foldable as Import (toList)
import Data.Function as Import (on)
import Data.Int as Import (Int64)
import Data.List (sortBy, (\\), nub)
import Data.Map as Import (Map)
import Data.Maybe as Import
            (fromMaybe, listToMaybe, mapMaybe, isJust, catMaybes)
import Data.Monoid as Import ((<>))
import Data.Set as Import (Set)
import Data.Text as Import (Text)
import Data.Time.Clock as Import (UTCTime, diffUTCTime, getCurrentTime)
import Database.Esqueleto as Import hiding (on, valList)
import Prelude as Import hiding (head, init, last, readFile, tail, writeFile)
import Yesod (languages)
import Yesod as Import
            hiding (Route (..)
                   ,(||.)
                   ,(==.)
                   ,(!=.)
                   ,(<.)
                   ,(<=.)
                   ,(>.)
                   ,(>=.)
                   ,(=.)
                   ,(+=.)
                   ,(-=.)
                   ,(*=.)
                   ,(/=.)
                   ,selectSource
                   ,delete
                   ,update
                   ,count
                   ,Value
                   ,runDB
                   ,languages)
import Yesod.Auth as Import
import Yesod.Form.Bootstrap3 as Import
import Yesod.Markdown as Import (Markdown (..))
import qualified Data.Text as T
import qualified Database.Esqueleto
import qualified Data.Map as M

import Foundation as Import
import Model as Import
import Model.Language as Import
import Model.Comment.Internal as Import hiding (TagName, TicketName)
import Model.Established.Internal as Import
import Model.Role.Internal as Import
import Model.SnowdriftEvent.Internal as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import

on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = Database.Esqueleto.on

-- Like Database.Esqueleto.valList, but more generic.
valList :: (Esqueleto query expr backend, PersistField typ, Foldable l) => l typ -> expr (ValueList typ)
valList = Database.Esqueleto.valList . toList

-- XXX: Will this always succeed?
key :: PersistEntity record => PersistValue -> Key record
key v = let Right k = keyFromValues [v] in k

entitiesMap :: Ord (Key t) => [Entity t] -> Map (Key t) t
entitiesMap = foldr (\(Entity k v) -> M.insert k v) mempty

-- | Convenience function for unwrapping an Entity and supplying both the key and value to another function.
onEntity :: (Key a -> a -> b) -> Entity a -> b
onEntity f (Entity x y) = f x y

{- The following footnote and toc functions were used our pre-wiki about page
At the time of this comment, they are no longer used anywhere live. -}

footnote :: Integer -> Widget
footnote note = [whamlet|$newline never
    <sup>
        <a name="fn_use#{show note}" href="#fn#{show note}">
            #{show note}
|]


footnoteAnchor :: String -> Widget
footnoteAnchor labels =
    case words labels of
        (first_label : remaining_labels) ->
            [whamlet|$newline never
                [
                    <a .footnote_anchor name="fn#{first_label}" href="#fn_use#{first_label}">
                        #{first_label}

                    $forall label <- remaining_labels
                        ,
                        <a .footnote_anchor name="fn#{label}" href="#fn_use#{label}">
                            #{label}
                ]
            |]

        _ -> error "empty footnote anchor"

tocEntry :: String -> String -> Widget
tocEntry tag title =
    [whamlet|$newline never
        <li .toc_li>
            <a .toc_entry name="toc_entry#{tag}" href="#toc_target#{tag}">
                #{title}
    |]


tocTarget :: String -> String -> Widget
tocTarget tag title =
    [whamlet|$newline never
        <span .title>
            #{title}
        <sup>
            <a .toc_target name="toc_target#{tag}" href="#toc_entry#{tag}" title="Back to Table Of Contents">
                ^
    |]

--------------------------------------------------------------------------------
-- Utility functions

lookupParamDefault :: Read a => Text -> a -> Handler a
lookupParamDefault name def = do
    maybe_param <- lookup name <$> reqGetParams <$> getRequest
    return $ fromMaybe def $ do
        param_str <- maybe_param
        param <- listToMaybe $ reads $ T.unpack param_str
        return $ fst param


getLanguages :: Handler [Language]
getLanguages = cached $ nub . mapMaybe fromPathPiece <$> languages


makeLanguageOptions :: Handler (OptionList Language)
makeLanguageOptions = do
    preferred_languages <- getLanguages

    app <- getYesod

    langs <- languages

    let render :: Language -> Text
        render = renderMessage app langs . MsgLangName

    return $ OptionList
        { olOptions = map (mkOption render) $ preferred_languages ++ (sortBy (compare `on` render) $ [minBound..maxBound] \\ preferred_languages)
        , olReadExternal = fromPathPiece
        }
  where
    mkOption render language = Option
            { optionDisplay = render language
            , optionInternalValue = language
            , optionExternalValue = toPathPiece language
            }

languagePreferenceOrder :: [Language] -> (a -> Language) -> a -> a -> Ordering
languagePreferenceOrder langs getLang = flip compare `on` (flip lookup (zip (reverse langs) [1 :: Integer ..]) . getLang)

pickTargetsByLanguage :: [Language] -> [Entity WikiTarget] -> [Entity WikiTarget]
pickTargetsByLanguage langs targets =
    let target_map = M.fromListWith (++) $ map (wikiTargetPage . entityVal &&& (:[])) targets
     in M.elems $ M.mapMaybe (listToMaybe . sortBy (languagePreferenceOrder langs (wikiTargetLanguage . entityVal))) target_map

pickEditsByLanguage :: [Language] -> [Entity WikiEdit] -> [Entity WikiEdit]
pickEditsByLanguage langs targets =
    let target_map = M.fromListWith (++) $ map (wikiEditPage . entityVal &&& (:[])) targets
     in M.elems $ M.mapMaybe (listToMaybe . sortBy (languagePreferenceOrder langs (wikiEditLanguage . entityVal))) target_map

--------------------------------------------------------------------------------
-- /
