module Dev
        ( toBeWrittenNotice
        , alphaRewriteNotice
        ) where

import Import

toBeWrittenNotice :: Widget
toBeWrittenNotice = $(widgetFile "dev/to-be-written")

alphaRewriteNotice :: Widget
alphaRewriteNotice = $(widgetFile "dev/alpha-rewrite")
