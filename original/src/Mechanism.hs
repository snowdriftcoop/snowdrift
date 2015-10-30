module Mechanism where

import Import hiding (Project, User)
import qualified Import as Fixme

import Data.Monoid (Sum(..))
import Data.Time.Clock (addUTCTime)

import Model.Currency

{- REFACTOR NOTES

 - moneyInfo

     SqlPersistT will hopefully go away and somehow refer to the main site.

     Maybe should probably also go away, since the caller can call
     (sequence . fmap) themselves. I actually set out to do that, but got
     clobbered with a type mismatch between "DB" and "ReaderT SqlBackend
     ..." — types that are actually the same.

  - Project and User

     These will have mechanism-specific information, including mapping to
     the master project's entities.

  - fetchUserPledgesDB

     Cut out with a chainsaw; please excuse the mess.
 -}

data Project = Project
data User = User

moneyInfo :: MonadIO m
          => Maybe (Entity Fixme.User)
          -> SqlPersistT m (Maybe (Milray, Milray))
moneyInfo = sequence . fmap go
  where
    go (Entity user_id user) = do
      (pledges, balance) <- goDB user_id user
      let pledged = getSum $ foldMap
              (\(project, pledge) ->
                  (Sum
                   . (projectShareValue (entityVal project) $*)
                   . fromIntegral
                   . pledgeFundedShares
                   . entityVal) pledge)
              pledges
      return (balance, pledged)
    goDB user_id user = do
        pledges :: [(Entity Fixme.Project, Entity Pledge)] <- select $ from $
            \(project `InnerJoin` pledge) -> do
                on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                where_ $ pledge ^. PledgeUser ==. val user_id
                return (project, pledge)
        Just account <- get (userAccount user)
        return
            ( pledges
            , accountBalance account)

fetchUserPledgesDB :: UserId -> DB [(Entity Fixme.Project, Project)]
fetchUserPledgesDB _user_id = return []

payoutHistory :: Fixme.Project
              -> UTCTime
              -> DB (Maybe (Milray, Milray, Milray))
payoutHistory project now = case projectLastPayday project of
    Nothing -> return Nothing
    Just last_payday -> do
        let extractRational = \case
                [Value (Just (r :: Rational))] -> r
                _                              -> 0
        -- This assumes there were transactions associated with the
        -- last payday
        last <- fmap extractRational $
            select $
            from $ \transaction -> do
            where_ $
                transaction ^. TransactionPayday ==. val (Just last_payday) &&.
                transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            return $ sum_ $ transaction ^. TransactionAmount

        year <- fmap extractRational $
            select $
            from $ \(transaction `InnerJoin` payday) -> do
            where_ $
                payday ^. PaydayDate >. val (addUTCTime (-365 * 24 * 60 * 60) now) &&.
                transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            on_ $ transaction ^. TransactionPayday ==. just (payday ^. PaydayId)
            return $ sum_ $ transaction ^. TransactionAmount

        total <- fmap extractRational $
            select $
            from $ \transaction -> do
            where_ $ transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            return $ sum_ $ transaction ^. TransactionAmount

        return $ Just (Milray $ round last, Milray $ round year, Milray $ round total)

fetchUser :: Fixme.UserId -> DB User
fetchUser = const (pure User)

fetchProject :: Fixme.ProjectId -> DB Project
fetchProject = const (pure Project)
