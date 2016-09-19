module Handler.Project (getSnowdriftProjectR) where

import Import

import Handler.TH

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = $(widget "page/snowdrift-project" "Snowdrift.coop Project")
