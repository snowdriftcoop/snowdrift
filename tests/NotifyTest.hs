{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module NotifyTest (notifySpecs) where

import TestImport

import Model.Notification

notifySpecs :: Spec
notifySpecs = do
    ydescribe "notifications" $ mapM_ testNotification [minBound .. maxBound]

  where
    testNotification NotifReply = yit "notifies correctly on reply" $ [marked|
            loginAs Mary
            
            -- TODO: flesh this out
        |]

    testNotification NotifWelcome = return () -- TODO
    testNotification NotifEligEstablish = return () -- TODO
    testNotification NotifBalanceLow = return () -- TODO
    testNotification NotifUnapprovedComment = return () -- TODO
    testNotification NotifApprovedComment = return () -- TODO
    testNotification NotifRethreadedComment = return () -- TODO
    testNotification NotifEditConflict = return () -- TODO
    testNotification NotifFlag = return () -- TODO
    testNotification NotifFlagRepost = return () -- TODO
