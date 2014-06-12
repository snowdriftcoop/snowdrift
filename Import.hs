{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies #-}

module Import ( module Import ) where


import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..), (||.), (==.), (!=.), (<.), (<=.), (>.), (>=.), (=.), (+=.), (-=.), (*=.), (/=.), selectSource, delete, update, count, Value)
import           Yesod.Auth           as Import
import           Yesod.Markdown       as Import (Markdown)
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           Control.Arrow        as Import ((***), (&&&), first, second)

import           Database.Esqueleto   as Import hiding (on)
import qualified Database.Esqueleto

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

import           Data.Function        as Import (on)

import           Data.Map             as Import (Map)
import           Data.Set             as Import (Set)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

import           Data.Maybe           as Import (fromMaybe, listToMaybe, mapMaybe, isJust, catMaybes)

import           Data.Int             as Import (Int64)

import           Control.Monad        as Import

import           Data.Time.Clock      as Import (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Units

import Control.Exception (Exception)
import Data.Typeable (Typeable)

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif


on_ :: Esqueleto query expr backend => expr (Value Bool) -> query ()
on_ = Database.Esqueleto.on


class Count a where
    getCount :: a -> Int64

data UserCount = UserCount Int64
instance Count UserCount where getCount (UserCount c) = c

data ShareCount = ShareCount Int64
instance Count ShareCount where getCount (ShareCount c) = c

newtype Color = Color Int deriving (Typeable, Num)

-- from http://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe
readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing


age :: UTCTime -> UTCTime -> String
age a b = let s = round $ toRational $ diffUTCTime a b
              f (t :: Second)
                 | t > convertUnit (1 :: Fortnight) = show (convertUnit t :: Fortnight)
                 | t > convertUnit (1 :: Week) = show (convertUnit t :: Week)
                 | t > convertUnit (1 :: Day) = show (convertUnit t :: Day)
                 | t > convertUnit (1 :: Hour) = show (convertUnit t :: Hour)
                 | otherwise = show (convertUnit t :: Minute)
           in f s


entityPairs :: [Entity t] -> [(Key t, t)]
entityPairs = map (\ (Entity a b) -> (a, b))

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

renderBootstrap3 :: Monad m => FormRender m a
renderBootstrap3 aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
                $newline never
                \#{fragment}
                $forall view <- views
                    <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                        $if not ( TL.null ( Text.Blaze.Html.Renderer.Text.renderHtml ( fvLabel view )))
                            <label for=#{fvId view}>#{fvLabel view}
                        ^{fvInput view}
                        $maybe tt <- fvTooltip view
                            <span .help-block>#{tt}
                        $maybe err <- fvErrors view
                            <span .help-block>#{err}
                |]
    return (res, widget)

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
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
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
        \theId name attrs value isReq -> do
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


getByErr message = runDB . fmap fromJustError . getBy
    where
        fromJustError :: Maybe a -> a
        fromJustError = fromMaybe (error message)


-- maybe we should make this a typeclass?
class WrappedValues a where
    type Unwrapped a
    unwrapValues :: a -> Unwrapped a

instance WrappedValues (Value a) where
    type Unwrapped (Value a) = a
    unwrapValues (Value v) = v

instance (WrappedValues a, WrappedValues b) => WrappedValues (a, b) where
    type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
    unwrapValues (a, b) = (unwrapValues a, unwrapValues b)

instance (WrappedValues a, WrappedValues b, WrappedValues c) => WrappedValues (a, b, c) where
    type Unwrapped (a, b, c) = (Unwrapped a, Unwrapped b, Unwrapped c)
    unwrapValues (a, b, c) = (unwrapValues a, unwrapValues b, unwrapValues c)




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
