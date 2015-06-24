module Widgets.Search where

import Import
import Data.Text

data SearchParameters = SearchParameters
    { claimed :: Bool
    , unclaimed :: Bool
    , tags :: Maybe Text
    }

searchWidget :: Html -> MForm Handler (FormResult SearchParameters, Widget)
searchWidget extra = do
    (claimedRes, claimedView) <- mreq checkBoxField "Claimed" Nothing
    (unclaimedRes, unclaimedView) <- mreq checkBoxField "Unclaimed" Nothing
    (tagsRes, tagsView) <- mopt textField "Tags" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> unclaimedRes
                    <*> tagsRes

    let widget = do
          [whamlet|
            #{extra}
            <div>
              <p>^{fvInput claimedView} Claimed
                 ^{fvInput unclaimedView} Unclaimed
              <p>Tags: ^{fvInput tagsView}
            <div>
          |]

    return (searchRes, widget)

--    where
--        checkBoxValField = (checkBool
--                               (\x -> validateField x claimedRes unclaimedRes)
--                               "Both \"Claimed Only\" and \"Unclaimed Only\"\
--                               \Checkboxes cannot be checked at the same time."
--                               checkBoxField)
--
--        validateField _ x y = not (x && y)

searchString :: SearchParameters -> Text
searchString (SearchParameters claimed unclaimed tags) = 
    if (claimed)
        then
            if (unclaimed)
                then append empty (fromMaybe empty tags)
                else append "CLAIMED " (fromMaybe empty tags)
        else
            if (unclaimed)
                then append "UNCLAIMED " (fromMaybe "" tags)
                else append empty (fromMaybe "" tags)

-- Note:  To add list of tags (assuming we can get to that point, don't forget
--        to use intercalate from Data.Text.  Although the "append empty" in
--        the last line is not needed, it's set up to remember to use 
--        intercalate later when I have Maybe [Text].
