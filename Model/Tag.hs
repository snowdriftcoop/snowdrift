module Model.Tag where

import Import

getAllTags :: YesodDB App [Entity Tag]
getAllTags = select $ from (\t -> return t)
