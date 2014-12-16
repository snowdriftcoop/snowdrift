module Widgets.Image (imageSelectField) where

import Import
import Yesod.Form.Functions (parseHelper)

imageSelectField :: [Entity Image] -> Field Handler Text   -- [Entity Image]
imageSelectField images = Field
    { fieldParse = parseHelper $ Right
-- \value _ -> case value of
--        ("":_) -> return $ Right Nothing
--        (x:_) -> return $ either (Left . SomeMessage) (Right . Just) x
    , fieldView = \idAttr nameAttr attrs result _ -> $(widgetFile "image_selector")
    , fieldEnctype = UrlEncoded
    }
