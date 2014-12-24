{-# LANGUAGE RecordWildCards #-}

module EmailTest (emailSpecs) where

import TestImport
import Model.Notification (NotificationType(..))

import Yesod.Default.Config (AppConfig (..), DefaultEnv (..))

emailSpecs :: AppConfig DefaultEnv a -> FilePath -> Spec
emailSpecs AppConfig {..} _ = do
    ydescribe "email notifications" $ do
        mapM_ testEmail [minBound .. maxBound]

  where
    -- Not delivered by email.
    testEmail NotifWelcome           = return ()

    testEmail NotifEligEstablish     = return ()

    -- XXX: Not triggered anywhere.
    testEmail NotifBalanceLow        = return ()

    -- XXX: Cannot be set by a user, so it should not be delivered by
    -- email.
    testEmail NotifUnapprovedComment = return ()

    -- XXX: Not triggered anywhere.
    testEmail NotifApprovedComment   = return ()

    testEmail NotifRethreadedComment = return ()

    testEmail NotifReply             = return ()

    -- XXX: TODO.
    testEmail NotifEditConflict      = return ()

    testEmail NotifFlag              = return ()
    testEmail NotifFlagRepost        = return ()
    testEmail NotifWikiPage          = return ()
    testEmail NotifWikiEdit          = return ()
    testEmail NotifBlogPost          = return ()
    testEmail NotifNewPledge         = return ()
    testEmail NotifUpdatedPledge     = return ()
    testEmail NotifDeletedPledge     = return ()
