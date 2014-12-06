module Widgets.Image (imageSelectField) where

import Import

imageSelectField :: [Entity Image] -> Maybe Text -> Field Handler Text   -- [Entity Image]
imageSelectField images prev_image = Field
    { fieldParse = \value _ ->
        case value of
            [a]
                | a == "None" -> return $ Right Nothing
                | otherwise -> return $ Right $ Just a
            [] -> return $ Right Nothing
            _ -> return $ Left "Error Parsing image" 
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> handlerToWidget $ defaultLayout $ do
            $(widgetFile "image_selector")

    , fieldEnctype = UrlEncoded
    }
