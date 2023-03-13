module Handler.Dashboard (getDashboardR) where

import Import

import Text.Blaze.Html (ToMarkup(..))

import Crowdmatch
import Handler.TH
import Handler.Pledge (pledgeForm)
import MarkupInstances ()

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    (patron, project) <- runDB $ (,) <$> fetchPatron uid <*> fetchProject
    (pledgeNoCSRF, _) <- generateFormPost (renderDivsNoLabels pledgeForm)
    let pendingDonation = patronDonationPayable patron
        crowdmatches = reverse $ map withMonthView (patronCrowdmatches patron)
    $(widget "page/dashboard" "Dashboard")
  where
    withMonthView (CrowdmatchDay d, amt) = (MonthView d, amt)

newtype MonthView = MonthView Day

instance ToMarkup MonthView where
    toMarkup (MonthView t) =
        toMarkup (formatTime defaultTimeLocale "%b %Y" t)
