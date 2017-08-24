module Handler.PaymentHistory (getPaymentHistoryR) where

import Import

import Crowdmatch
import Handler.TH
import MarkupInstances ()

getPaymentHistoryR :: Handler Html
getPaymentHistoryR = do
    Entity uid _ <- requireAuth
    (patron, project) <- runDB $ (,) <$> fetchPatron uid <*> fetchProject
    $(widget "page/payment-history" "Payment History")
