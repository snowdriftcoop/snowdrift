module Widgets.Image (imageSelectField) where

import Import

-- | This shows all images in the image table of the db to allow a user to
-- select an image for purposes of selecting a project logo or user avatar.
--
-- FUTURE WORK: At this point, there is no way to filter, as the Image
-- table & image uploader page need to be enhanced first.  Once done, this
-- widget will need to be updated to allow for filtering & sorting.
imageSelectField :: [Entity Image] -> Field Handler Text
imageSelectField images = Field
    { fieldParse = \ e _ -> return $ imageParser e
    , fieldView = \ idAttr nameAttr attrs result _ ->
        $(widgetFile "image_selector")
    , fieldEnctype = UrlEncoded
    }
    where
        imageParser [] = Right Nothing
        imageParser (x:_) = case x of
            "" -> Right Nothing
            "None" -> Right Nothing
            t -> Right $ Just t
