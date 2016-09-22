module Handler.Pledge (postPledgeSnowdriftR) where

import Import

import Alerts

postPledgeSnowdriftR :: Handler Html
postPledgeSnowdriftR = do
    Entity uid _ <- requireAuth
    dbres <- runDB $ do
        p <- getBy (UniquePledge uid)
        maybe (pledge' uid) feignHorror p
    case dbres of
        Just t -> alertInfo
            [shamlet|Your pledge that started on #{show t} is still valid.|]
        Nothing -> alertSuccess "You are now pledged!"
    redirect SnowdriftProjectR
  where
    pledge' uid = do
        now <- liftIO getCurrentTime
        insert_ (Pledge uid now)
        insert_ (PledgeHistory uid now MakePledge)
        pure Nothing
    feignHorror (Entity _ Pledge{..}) = pure (Just _pledgeSince)
