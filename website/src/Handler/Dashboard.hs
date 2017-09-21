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

data DashboardPage = DashboardPledges | DashboardPaymentInfo
    deriving (Eq)

dashboardHeader :: DashboardPage -> Patron -> Project -> Widget
dashboardHeader activePage patron project =
    let -- list of dashboard pages with routes and titles
        pages :: [(DashboardPage,Route App,Text)]
        pages =
            [ (DashboardPledges,DashboardR,"Pledges")
            , (DashboardPaymentInfo,PaymentInfoR,"Payment System")
            ]
    in $(widgetFile "dashboard-header")

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    (patron, project) <- runDB $ (,) <$> fetchPatron uid <*> fetchProject
    (pledgeNoCSRF, _) <- generateFormPost (renderDivsNoLabels pledgeForm)
    $(widget "page/dashboard" "Dashboard")

getPaymentInfoR :: Handler Html
getPaymentInfoR = do
    getPaymentInfo (dashboardHeader DashboardPaymentInfo)
