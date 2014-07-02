module Model.Tag where

import Import

type TagMap = Map TagId Tag

getAllTags :: YesodDB App [Entity Tag]
getAllTags = select $ from (\t -> return t)
