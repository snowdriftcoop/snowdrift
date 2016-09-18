module Handler.Dashboard where

import Import

import Handler.TH

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    (_pledgeActs, mpledge) <- runDB (do
        acts <- selectList [PledgeHistoryUsr ==. uid] [Asc PledgeHistoryTime]
        p <- getBy (UniquePledge uid)
        pure (acts, p))
    $(widget "page/dashboard" "Dashboard")
