-- | This module defines methods needed for the YesodAuthEmail instance for
-- App.
module EmailAuth
        ( addUnverified
        , getVerifyKey
        , setVerifyKey
        , verifyAccount
        , getPassword
        , setPassword
        , getEmailCreds
        , getEmail
        ) where

import Import.NoFoundation

import Control.Monad.Trans.Maybe (MaybeT(..))
import Yesod.Auth.Email (EmailCreds(..), Email, VerKey, SaltedPass, AuthEmailId)

-- | Add a new, unverified user. Uniqueness guarantees this blows up
-- if the user already exists.
addUnverified :: MonadIO m => Email -> VerKey -> SqlPersistT m VerifyEmailId
addUnverified e k = do
    uid <- insert (User e False Nothing)
    insert (VerifyEmail k uid)

getVerifyKey :: MonadIO m => VerifyEmailId -> SqlPersistT m (Maybe VerKey)
getVerifyKey eid = fmap (fmap verifyEmailKey) (get eid)

setVerifyKey :: MonadIO m => VerifyEmailId -> VerKey -> SqlPersistT m ()
setVerifyKey eid k = update eid [VerifyEmailKey =. k]

verifyAccount :: MonadIO m => VerifyEmailId -> SqlPersistT m (Maybe UserId)
verifyAccount eid = traverse doUpdate =<< get eid
  where
    doUpdate ver = do
        update (verifyEmailUser ver) [UserEmailVerified =. True]
        delete eid
        pure (verifyEmailUser ver)

getPassword :: MonadIO m => UserId -> SqlPersistT m (Maybe Text)
getPassword = fmap (userPassword =<<) . get

setPassword :: MonadIO m => UserId -> SaltedPass -> SqlPersistT m ()
setPassword uid saltedpass = update uid [UserPassword =. Just saltedpass]

-- | Since we only use email for identification, this function is
-- degenerate in a few ways.
getEmailCreds :: ( MonadIO m
                 , AuthEmailId site ~ Key VerifyEmail
                 , AuthId site ~ Key User)
              => Text
              -> SqlPersistT m (Maybe (EmailCreds site))
getEmailCreds ident = do
    muser <- getBy (UniqueUsr ident)
    case muser of
        Just (Entity uid _) -> do
            mver <- getBy (UniqueVerify uid)
            pure (makeCreds <$> muser <*> mver)
        Nothing -> pure Nothing
  where
    makeCreds (Entity uid user) (Entity vid ver) =
        EmailCreds
            vid
            (Just uid)
            (userEmailVerified user)
            (Just (verifyEmailKey ver))
            (userEmail user)

getEmail :: MonadIO m => VerifyEmailId -> SqlPersistT m (Maybe Email)
getEmail eid = runMaybeT $ do
    ver <- MaybeT $ get eid
    user <- MaybeT $ get (verifyEmailUser ver)
    pure (userEmail user)
