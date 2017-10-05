module Handler.Dashboard (getDashboardR) where

import Import

import Crowdmatch
import Handler.TH
import Handler.Pledge (pledgeForm)
import MarkupInstances ()

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    (patron, project) <- runDB $ (,) <$> fetchPatron uid <*> fetchProject
    (pledgeNoCSRF, _) <- generateFormPost (renderDivsNoLabels pledgeForm)
    let ten = [1..10]
    $(widget "page/dashboard" "Dashboard")
