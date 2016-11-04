module Handler.Dashboard (getDashboardR) where

import Import

import Crowdmatch
import Handler.TH
import MarkupInstances

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid user <- requireAuth
    patron <- fetchPatron runDB uid
    project <- fetchProject runDB
    $(widget "page/dashboard" "Dashboard")
