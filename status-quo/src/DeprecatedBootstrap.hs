-- | (Original description): allow easier creation of pretty bootstrap 3 forms. there has to be an easier way -_-
--
-- See SD-463.
module DeprecatedBootstrap where

import Import

import qualified Data.Text as T

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
