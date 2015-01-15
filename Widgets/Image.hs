module Widgets.Image (imageSelectField) where

import Import

imageSelectField :: [Entity Image] -> Field Handler Text   -- [Entity Image]
imageSelectField images = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \idAttr nameAttr attrs result _ -> $(widgetFile "image_selector")
    , fieldEnctype = UrlEncoded
    }
