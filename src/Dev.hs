module Dev
        ( toBeWrittenNotice
        ) where

import Import

toBeWrittenNotice :: Widget
toBeWrittenNotice = $(widgetFile "dev/to-be-written")
