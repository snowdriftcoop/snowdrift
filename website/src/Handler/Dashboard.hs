module Handler.Dashboard
        ( getDashboardR
        , getPaymentInfoR
        , postPaymentInfoR
        , deletePaymentInfoR
        ) where

import Import

import Crowdmatch
import Handler.TH
import Handler.Pledge (pledgeForm)
import MarkupInstances ()

import Handler.Dashboard.PaymentInfo

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    (patron, project) <- runDB $ (,) <$> fetchPatron uid <*> fetchProject
    (pledgeNoCSRF, _) <- generateFormPost (renderDivsNoLabels pledgeForm)
    $(widget "page/dashboard" "Dashboard")
