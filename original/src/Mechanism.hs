module Mechanism
        (moneyInfo
        ) where

import Import

import Data.Monoid (Sum(..))

import Model.Currency

{- REFACTOR NOTES
 -
 - SqlPersistT will hopefully go away and somehow refer to the main site.
 -
 - Maybe should probably also go away, since the caller can call (sequence
 - . fmap) themselves. I actually set out to do that, but got clobbered
 - with a type mismatch between "DB" and "ReaderT SqlBackend ..." — types
 - that are actually the same.
 -}

moneyInfo :: MonadIO m
          => Maybe (Entity User)
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
        pledges :: [(Entity Project, Entity Pledge)] <- select $ from $
            \(project `InnerJoin` pledge) -> do
                on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                where_ $ pledge ^. PledgeUser ==. val user_id
                return (project, pledge)
        Just account <- get (userAccount user)
        return
            ( pledges
            , accountBalance account)
