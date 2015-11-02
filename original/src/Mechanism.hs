{- REFACTOR NOTES

 - In general (but not always), I am importing this module qualified for
   now as an explicit reminder. Eventually it should be unqualified like most
   internal modules.

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

module Mechanism where

import Import hiding (Project, User)
import qualified Import as Fixme

import Control.Monad.Trans.Resource (MonadThrow)
import Control.Monad.Trans.Writer.Strict (tell, execWriterT, WriterT)
import Data.Either (isRight)
import Data.Monoid (Sum(..))
import Data.Time.Clock (addUTCTime)
import qualified Data.Map as M
import qualified Data.Text as T

import Model.Currency
import WrappedValues

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

potentialPledge :: Fixme.UserId
                -> Fixme.ProjectId
                -> DB (Maybe (Entity Pledge), [Int64], [Int64])
potentialPledge user_id project_id = do
    pledges <- fetchProjectSharesDB project_id
    mpledge <- getBy $ UniquePledge user_id project_id
    other_shares <- fmap unwrapValues $ select $ from $ \p -> do
        where_ $ p ^. PledgeProject ==. val project_id
            &&. p ^. PledgeUser !=. val user_id
        return $ p ^. PledgeShares
    return (mpledge, other_shares, pledges)

fetchProjectSharesDB :: ( MonadThrow m
                        , MonadIO m
                        , MonadBaseControl IO m
                        , MonadLogger m
                        , MonadResource m)
                     => ProjectId -> SqlPersistT m [Int64]
fetchProjectSharesDB project_id = do
    pledges <-
        select $
        from $ \pledge -> do
        where_ $
            pledge ^. PledgeProject ==. val project_id
            &&. pledge ^. PledgeFundedShares >. val 0
        return pledge
    return $ map (pledgeFundedShares . entityVal) pledges

-- signature needs to remain generic, for SnowdriftProcessPayments
updateShareValue
    :: ( MonadBaseControl IO m
       , MonadLogger m
       , MonadResource m)
    => ProjectId
    -> SqlPersistT m ()
updateShareValue project_id = do
    pledges <- fetchProjectSharesDB project_id
    update $ \project -> do
        set project
            [ ProjectShareValue =. val (projectComputeShareValue pledges) ]
        where_ (project ^. ProjectId ==. val project_id)

-- | Project's monthly share value: 0.1¢ × number of patrons.
projectComputeShareValue :: [Int64] -> Milray
projectComputeShareValue patronPledgeLevel =
    Milray 10 $* (fromIntegral $ length $ filter (/= 0) patronPledgeLevel)

-- | Keep dropping shares, until there are no underfunded patrons.
-- (Recursion alert.)
dropAllUnderfunded :: DBConstraint m
                   => ProjectId -> WriterT DropShares (SqlPersistT m) ()
dropAllUnderfunded projId = do
    -- Update share value before each run.
    lift $ updateShareValue projId
    unders <- lift $ decrementUnderfunded projId
    unless (null unders) $ do
        tell unders
        dropAllUnderfunded projId

newtype DropShare = DropShare PledgeId
type DropShares = [DropShare]

-- | Drop one share from each pledge.
dropShares :: MonadIO m => [PledgeId] -> SqlPersistT m ()
dropShares [] = return ()
dropShares ps =
    update $ \p -> do
    set p [ PledgeFundedShares -=. val 1 ]
    where_ $ p ^. PledgeId `in_` valList ps

-- | Find pledges in a given project (if supplied), from a given set of
-- users, that have the greatest number of shares (greater than 0)
maxShares :: (MonadIO m, Functor m)
          => Maybe ProjectId
          -> [UserId]
          -> SqlPersistT m [PledgeId]
maxShares _     []   = return []
maxShares mproj uids = do
    -- select...max_ :: m [Value (Maybe a)]
    [Value mmaxCt] <-
        select $
        from $ \p -> do
        where_ $ (p ^. PledgeUser `in_` valList uids)
            &&. p ^. PledgeFundedShares >. val 0
        return $ max_ $ p ^. PledgeFundedShares

    case mmaxCt of
        Nothing -> return []
        Just maxCt -> do
            let projConstraint pledge = case mproj of
                    Just proj -> pledge ^. PledgeProject ==. val proj
                    _         -> val True

            fmap unwrapValues $
                select $
                from $ \p -> do
                where_ $ (p ^. PledgeUser `in_` valList uids)
                    &&. projConstraint p
                    &&. p ^. PledgeFundedShares ==. val maxCt
                return $ p ^. PledgeId

-- | Find underfunded patrons.
underfundedPatrons :: (MonadIO m, Functor m)
                   => SqlPersistT m [UserId]
underfundedPatrons = do
    -- :: DB [(UserId, Milray, Int64)]
    pledgeList <- fmap unwrapValues $
        select $
        from $ \(usr `InnerJoin` plg `InnerJoin` prj) -> do
        on_ $ prj ^. ProjectId  ==. plg ^. PledgeProject
        on_ $ plg ^. PledgeUser ==. usr ^. UserId
        return
            ( usr ^. UserId
            , prj ^. ProjectShareValue
            , plg ^. PledgeFundedShares
            )

    let uids = map (\(i,_,_) -> i) pledgeList

    -- :: DB (Map UserId Milray)
    balances <- fmap (M.fromList . unwrapValues) $
        select $
        from $ \(u `InnerJoin` a) -> do
        on_ $ u ^. UserAccount ==. a ^. AccountId
        where_ $ u ^. UserId `in_` valList uids
        return (u ^. UserId, a ^. AccountBalance)

    -- Sum outlays over the pledge list.
    let userOutlays :: M.Map UserId Milray
        userOutlays = getSum <$> foldMap outlaySum pledgeList

    -- Filter out non-negative (balance - outlay) and return
    return $ M.keys $ M.differenceWith maybeNegSubtract balances userOutlays

  where

    -- | Create something with a summable type.
    outlaySum :: (UserId, Milray, Int64) -> M.Map UserId (Sum Milray)
    outlaySum (u, shareValue, fundedShares) =
        M.singleton u (Sum $ fromIntegral fundedShares *$ shareValue)

    -- | Given "a - b", return just the absolute value (≡ b - a) if the
    -- difference is negative.
    maybeNegSubtract :: (Ord s, Num s) => s -> s -> Maybe s
    maybeNegSubtract a b
        | a < b     = Just $ b - a
        | otherwise = Nothing

-- | Drop one share from each highest-shared underfunded pledges to a
-- particular project, and update the project share value. Return which
-- ones got dropped.
decrementUnderfunded :: ProjectId -> DB DropShares
decrementUnderfunded projId = do
    droppers <- join $ maxShares (Just projId) <$> underfundedPatrons
    dropShares droppers
    return $ map DropShare droppers

-- | Fold some DropShares into EventDeactivatedPledges, one per affected
-- pledge.
--
-- To be implemented for SD-603.
foldDrops :: UTCTime -> DropShares -> [SnowdriftEvent]
foldDrops _ts = map snd . toList . foldr insertOrAdd M.empty
  where
    insertOrAdd = flip const

-- | After a patron modifies their pledge, some other patrons may be
-- underfunded. This method deactivates shares from those underfunded
-- pledges until all pledges are funded.
rebalanceProjectPledges :: ProjectId -> SYDB ()
rebalanceProjectPledges project_id = do
    allDrops <- lift . execWriterT $ dropAllUnderfunded project_id
    now <- liftIO getCurrentTime
    tell $ foldDrops now allDrops

updateUserPledge :: Text -> Int64 -> HandlerT App IO ()
updateUserPledge project_handle shares = do
    Just pledge_render_id <-
        fmap (read . T.unpack) <$> lookupSession pledgeRenderKey

    status <- runSYDB $ do
        Entity user_id user <- lift (lift requireAuth)
        Just account <- lift $ get (userAccount user)
        Entity project_id project <-
            lift $ getBy404 (UniqueProjectHandle project_handle)
        mold_shares <- lift $ do
            mpledge <- getBy $ UniquePledge user_id project_id
            return $ case mpledge of
                Nothing                -> Nothing
                Just (Entity _ pledge) -> Just (pledgeFundedShares pledge)
        let mnew_shares  = if shares == 0 then Nothing else Just shares
            user_outlay  = projectShareValue project $* fromIntegral shares
            enough_money = accountBalance account >= user_outlay $* 3
            -- At the time of writing this comment, pledging
            -- multiple times breaks 'renderProject' and
            -- 'SnowdriftProcessPayments'.  In any case, there
            -- is no need to allow pledging the same amount
            -- multiple times ever.
            new_amount   = mold_shares /= mnew_shares

            pledge_dropped   = "You have dropped your pledge and are no longer "
                            <> "a patron of " <> projectName project <> "."
            pledge_updated   = "You have now pledged a base rate of "
                            <> (T.pack $ show $ millMilray shares)
                            <> " per patron. "
                            <> "Thank you for supporting "
                            <> projectName project <> "!"
            same_amount      = "you cannot pledge the same amount"
            not_enough_money = "you must have funds to support your pledge "
                            <> "for at least 3 months at current pledge value. "
                            <> "Please deposit additional funds to your account"
            status = case (enough_money, new_amount) of
                (True, True)   ->
                    if shares == 0
                        then Right pledge_dropped
                        else Right pledge_updated
                (True, False)  ->
                    Left $ "Sorry, " <> same_amount <> "."
                (False, True)  ->
                    Left $ "Sorry, " <> not_enough_money <> "."
                (False, False) ->
                    Left $ "Sorry, " <> same_amount <> " and "
                        <> not_enough_money <> "."
            success = isRight status

        when success $ do
            insertProjectPledgeDB
                user_id
                project_id
                shares
                pledge_render_id
            rebalanceProjectPledges project_id

        return status

    case status of
        Right msg -> alertSuccess msg
        Left  msg -> alertWarning msg

pledgeRenderKey :: Text
pledgeRenderKey = "pledge_render"

insertProjectPledgeDB :: UserId
                      -> ProjectId
                      -> Int64
                      -> PledgeFormRenderedId
                      -> SDB ()
insertProjectPledgeDB user_id project_id shares pledge_render_id = do
    now <- liftIO getCurrentTime
    let shares_pledged =
            SharesPledged now user_id project_id shares pledge_render_id
    shares_pledged_id <- lift (insert shares_pledged)
    lift (getBy (UniquePledge user_id project_id)) >>= \case
        Nothing -> do
            lift $ insert_ (Pledge now user_id project_id shares shares)
            tell [ENewPledge shares_pledged_id shares_pledged]
        Just (Entity pledge_id old_pledge) -> do
            if shares == 0
                then do
                    lift (deleteKey pledge_id)
                    tell [EDeletedPledge now
                                         user_id
                                         project_id
                                         (pledgeShares old_pledge)]
                else do
                    lift $
                        update $ \p -> do
                        set p [ PledgeShares       =. val shares
                              , PledgeFundedShares =. val shares
                              ]
                        where_ (p ^. PledgeId ==. val pledge_id)
                    tell [EUpdatedPledge (pledgeShares old_pledge)
                                         shares_pledged_id
                                         shares_pledged]
