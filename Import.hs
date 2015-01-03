{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import (module Import) where

import           Foundation                    as Import
import           Model                         as Import
import           Model.Language                as Import
import           Model.Comment.Internal        as Import
import           Model.Established.Internal    as Import
import           Model.Role.Internal           as Import
import           Model.SnowdriftEvent.Internal as Import
import           Settings                      as Import
import           Settings.Development          as Import
import           Settings.StaticFiles          as Import

import           Control.Applicative           as Import (pure, (<$>), (<*>))
import           Control.Arrow                 as Import ((***), (&&&), (+++), first, second, (>>>), (<<<))
import           Control.Monad                 as Import
import           Data.Function                 as Import (on)
import           Data.Int                      as Import (Int64)
import           Data.Map                      as Import (Map)
import           Data.Maybe                    as Import (fromMaybe, listToMaybe, mapMaybe, isJust, catMaybes)
import           Data.Set                      as Import (Set)
import           Data.Text                     as Import (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               as Import (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Typeable (Typeable)
import           Database.Esqueleto            as Import hiding (on, valList)
import qualified Database.Esqueleto
import           Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import           Network.Mail.Mime               (randomString)
import           Prelude                       as Import hiding (head, init, last, readFile, tail, writeFile)
import           System.Random                 (newStdGen)
import           Yesod                         as Import hiding (Route (..), (||.), (==.), (!=.), (<.), (<=.), (>.), (>=.), (=.), (+=.), (-=.), (*=.), (/=.), selectSource, delete, update, count, Value, runDB, languages)
import           Yesod.Auth                    as Import
import           Yesod.Markdown                as Import (Markdown)
import           Yesod.Form.Bootstrap3         as Import

import           Yesod (languages)
import           Data.List (sortBy, (\\), nub)

import GHC.Exts (IsList(..))
import qualified Data.Map as M
import qualified Data.Set as S

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

instance Ord a => IsList (Set a) where
    type Item (Set a) = a
    fromList = S.fromList
    toList = S.toList

on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = Database.Esqueleto.on

-- Like Database.Esqueleto.valList, but more generic.
valList :: (Esqueleto query expr backend, PersistField typ, IsList l, typ ~ Item l) => l -> expr (ValueList typ)
valList = Database.Esqueleto.valList . toList

infix 4 `notDistinctFrom`
notDistinctFrom :: SqlExpr (Value a) -> SqlExpr (Value a)
                -> SqlExpr (Value Bool)
notDistinctFrom = unsafeSqlBinOp " IS NOT DISTINCT FROM "

selectCount :: (MonadResource m, MonadSqlPersist m) => SqlQuery a -> m Int
selectCount from_ =
    fmap (\[Value n] -> n) $
    select $ from_ >> return countRows

selectExists :: SqlQuery a -> DB Bool
selectExists query = selectCount query >>= \n -> return $ n > 0

newHash :: IO Text
newHash = T.pack . fst . randomString 42 <$> newStdGen

class Count a where
    getCount :: a -> Int64

data UserCount = UserCount Int64
instance Count UserCount where getCount (UserCount c) = c

data ShareCount = ShareCount Int64
instance Count ShareCount where getCount (ShareCount c) = c

newtype Color = Color Int deriving (Typeable, Num)

showDiffTime :: UTCTime -> UTCTime -> String
showDiffTime x y =
  let secs_ago = round (diffUTCTime x y)
  in if | secs_ago < secsPerHour  -> go secs_ago secsPerMinute "m"
        | secs_ago < secsPerDay   -> go secs_ago secsPerHour   "h"
        | secs_ago < secsPerWeek  -> go secs_ago secsPerDay    "d"
        | secs_ago < secsPerMonth -> go secs_ago secsPerWeek   "wk"
        | secs_ago < secsPerYear  -> go secs_ago secsPerMonth  "mo"
        | otherwise               -> go secs_ago secsPerYear   "yr"
  where
    go secs_ago divisor suffix = show (secs_ago `div` divisor) ++ suffix

    secsPerMinute, secsPerHour, secsPerDay, secsPerWeek, secsPerMonth, secsPerYear :: Integer
    secsPerMinute = 60
    secsPerHour   = 3600     -- 60*60
    secsPerDay    = 86400    -- 60*60*24
    secsPerWeek   = 604800   -- 60*60*24*7
    secsPerMonth  = 2592000  -- 60*60*24*30
    secsPerYear   = 31536000 -- 60*60*24*365

entitiesMap :: [Entity t] -> Map (Key t) t
entitiesMap = foldr (\(Entity k v) -> M.insert k v) mempty

-- allow easier creation of pretty bootstrap 3 forms. there has to be an easier way -_-
--fieldSettings :: forall master . SomeMessage master -> [(Text, Text)] -> FieldSettings master
--fieldSettings label attrs = FieldSettings label Nothing Nothing Nothing attrs

aopt' :: MonadHandler m
    => Field m a
    -> SomeMessage (HandlerSite m)
    -> Maybe (Maybe a)
    -> AForm m (Maybe a)
aopt' a b = aopt a (FieldSettings b Nothing Nothing Nothing [("class", "form-control")])

areq' :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => Field m a
    -> SomeMessage site
    -> Maybe a
    -> AForm m a
areq' a b = areq a (FieldSettings b Nothing Nothing Nothing [("class", "form-control")])


radioField' :: (Eq a, RenderMessage site FormMessage)
           => HandlerT site IO (OptionList a)
           -> Field (HandlerT site IO) a
radioField' = selectFieldHelper'
    (\theId _name _attrs inside -> [whamlet|
$newline never
<div ##{theId}>^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<div .radio>
    <label for=#{theId}-none>
            <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
            _{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
$newline never
<div .radio>
    <label for=#{theId}-#{value}>
            <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
            \#{text}
|])

selectFieldHelper'
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetT site IO () -> WidgetT site IO ())
        -> (Text -> Text -> Bool -> WidgetT site IO ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetT site IO ())
        -> HandlerT site IO (OptionList a)
        -> Field (HandlerT site IO) a
selectFieldHelper' outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs value isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts value `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts value) == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y

optionsPairs' :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
             => (a -> String) -> [(msg, a)] -> m (OptionList a)
optionsPairs' mk_external opts = do
  mr <- getMessageRender
  let mkOption (display, internal) =
          Option { optionDisplay       = mr display
                 , optionInternalValue = internal
                 , optionExternalValue = T.pack $ mk_external internal
                 }
  return $ mkOptionList (map mkOption opts)

checkboxesFieldList' :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                     => (a -> String)
                     -> [(msg, a)]
                     -> Field (HandlerT site IO) [a]
checkboxesFieldList' mk_external = checkboxesField' . (optionsPairs' mk_external)

checkboxesField' :: (Eq a, RenderMessage site FormMessage)
                 => HandlerT site IO (OptionList a)
                 -> Field (HandlerT site IO) [a]
checkboxesField' ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs value _ -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist

            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals

            [whamlet|
                <span ##{theId}>
                    $forall opt <- opts
                        <input type=checkbox id="#{name}_#{optionExternalValue opt}" name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected value opt:checked>
                        <label for="#{name}_#{optionExternalValue opt}">
                            #{optionDisplay opt}
                |]
    }


redirectParams :: (MonadHandler (HandlerT site m), MonadBaseControl IO m) => Route site -> [(Text, Text)] -> HandlerT site m a
redirectParams route params = getUrlRenderParams >>= \ render -> redirect $ render route params

getByErr :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
         => String -> Unique val -> Handler (Entity val)
getByErr message = runYDB . fmap fromJustError . getBy
    where
        fromJustError :: Maybe a -> a
        fromJustError = fromMaybe (error message)

lookupErr :: Ord k => String -> k -> Map k a -> a
lookupErr = M.findWithDefault . error

fromJustErr :: String -> Maybe a -> a
fromJustErr _   (Just x) = x
fromJustErr msg _        = error msg

class WrappedValues a where
    type Unwrapped a
    unwrapValues :: a -> Unwrapped a

instance WrappedValues (Entity a) where
    type Unwrapped (Entity a) = (Entity a)
    unwrapValues = id

instance WrappedValues (Value a) where
    type Unwrapped (Value a) = a
    unwrapValues (Value v) = v

instance (WrappedValues a, WrappedValues b) => WrappedValues (a, b) where
    type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
    unwrapValues (a, b) = (unwrapValues a, unwrapValues b)

instance (WrappedValues a, WrappedValues b, WrappedValues c) => WrappedValues (a, b, c) where
    type Unwrapped (a, b, c) = (Unwrapped a, Unwrapped b, Unwrapped c)
    unwrapValues (a, b, c) = (unwrapValues a, unwrapValues b, unwrapValues c)

instance WrappedValues a => WrappedValues [a] where
    type Unwrapped [a] = [Unwrapped a]
    unwrapValues = map unwrapValues

instance WrappedValues a => WrappedValues (Maybe a) where
    type Unwrapped (Maybe a) = Maybe (Unwrapped a)
    unwrapValues = fmap unwrapValues

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
