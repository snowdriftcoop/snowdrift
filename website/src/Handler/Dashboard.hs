module Handler.Dashboard (getDashboardR) where

import Import

import Crowdmatch
import Handler.TH
import Handler.Pledge (pledgeForm)
import MarkupInstances ()

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    patron <- fetchPatron runDB uid
    project <- fetchProject runDB
    (pledgeNoCSRF, _) <- generateFormPost (renderDivsNoLabels pledgeForm)
    $(widget "page/dashboard" "Dashboard")
