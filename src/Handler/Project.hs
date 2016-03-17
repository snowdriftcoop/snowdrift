{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import Data.List (sortBy)
import System.Random (randomIO)
import Text.Cassius (cassiusFile)
import Text.Printf
import Yesod.AtomFeed
import Yesod.RssFeed
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Time.Format
import Dev
import Handler.Utils
import Model.Application
import Model.Currency
import Model.Project
import Model.Role
import Model.Shares
import Model.SnowdriftEvent
import Model.User
import View.Project
import View.SnowdriftEvent
import View.Time
import qualified Mechanism as Mech

--------------------------------------------------------------------------------
-- Utility functions

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def_val = do
    maybe_value <- lookupGetParam name
    return (fromMaybe def_val (maybe_value >>= readMaybe . T.unpack))

-- | Require any of the given Roles, failing with permissionDenied if none are satisfied.
requireRolesAny :: [Role] -> Text -> Text -> Handler (UserId, Entity Project)
requireRolesAny roles project_handle err_msg = do
    user_id <- requireAuthId

    (project, ok) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)

        ok <- userHasRolesAnyDB roles user_id project_id

        return (project, ok)

    unless ok $
        permissionDenied err_msg

    return (user_id, project)

--------------------------------------------------------------------------------
-- /applications (List of submitted applications)

getApplicationsR :: Text -> Handler Html
getApplicationsR project_handle = do
    viewer_id <- requireAuthId

    (project, applications) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        applications <- fetchProjectVolunteerApplicationsDB project_id
        userReadVolunteerApplicationsDB viewer_id
        return (project, applications)

    defaultLayoutNew "applications" $ do
        snowdriftTitle $ projectName project <> " Volunteer Applications"
        $(widgetFile "applications")

--------------------------------------------------------------------------------
-- /application (Form for new application)

getApplicationR :: Text -> VolunteerApplicationId -> Handler Html
getApplicationR project_handle application_id = do
    viewer_id <- requireAuthId
    (project, user, application, interests, num_interests) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        application <- get404 application_id
        let user_id = volunteerApplicationUser application
        user <- get404 user_id
        (interests, num_interests) <- (T.intercalate ", " &&& length) <$> fetchApplicationVolunteerInterestsDB application_id
        return (project, Entity user_id user, application, interests, num_interests)

    defaultLayoutNew "application" $ do
        snowdriftDashTitle
            (projectName project <> " Volunteer Application")
            (userDisplayName user)
        $(widgetFile "application")

--------------------------------------------------------------------------------
-- /edit

getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    (_, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    tags <- runDB $
        select $
        from $ \(p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayoutNew "edit-project" $ do
        snowdriftTitle $ projectName project
        $(widgetFile "edit-project")

--------------------------------------------------------------------------------
-- /feed

-- | This function is responsible for hitting every relevant event table. Nothing
-- statically guarantees that.
getProjectFeedR :: Text -> Handler TypedContent
getProjectFeedR project_handle = do
    let lim = 26 -- limit 'lim' from each table, then take 'lim - 1'

    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    before <- lookupGetUTCTimeDefaultNow "before"

    (
        project_id, project,
        is_watching,
        new_pledge_events,
        updated_pledge_events, deleted_pledge_events,

        user_map
     ) <- runYDB $ do

        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        is_watching <- maybe (pure False) (flip userIsWatchingProjectDB project_id) muser_id
        ( new_pledge_events
            , updated_pledge_events
            , deleted_pledge_events
            , pledging_users
            , unpledging_users) <- Mech.projectEvents project_id before lim

        -- Suplementary maps for displaying the data. If something above requires extra
        -- data to display the project feed row, it MUST be used to fetch the data below!

        let -- All users: comment posters, wiki page creators, etc.
            user_ids = S.toList $ mconcat
                        [ S.fromList pledging_users
                        , S.fromList unpledging_users
                        ]

        user_map <- entitiesMap <$> selectList [UserId <-. user_ids] []

        return
            (
                project_id, project,
                is_watching,
                new_pledge_events, updated_pledge_events, deleted_pledge_events,

                user_map
            )

    let all_unsorted_events :: [(Route App, SnowdriftEvent)]
        all_unsorted_events = mconcat
            [ map (EventNewPledgeR          *** onEntity ENewPledge)            new_pledge_events

            , map (\(eid, shares, pledge)
                    -> (EventUpdatedPledgeR eid, eup2se shares pledge))         updated_pledge_events

            , map (EventDeletedPledgeR      *** edp2se)                         deleted_pledge_events
            ]

        (events, more_events) = splitAt (lim-1) (sortBy (snowdriftEventNewestToOldest `on` snd) all_unsorted_events)

        -- For pagination: Nothing means no more pages, Just time means set the 'before'
        -- GET param to that time. Note that this means 'before' should be a <= relation,
        -- rather than a <.
        mnext_before :: Maybe Text
        mnext_before = case more_events of
          []             -> Nothing
          ((_, next_event):_) -> (Just . T.pack . show . snowdriftEventTime) next_event

    now        <- liftIO getCurrentTime
    Just route <- getCurrentRoute
    render     <- getUrlRender

    let feed = Feed "project feed" route HomeR "Snowdrift Community" "" "en" now Nothing $
            mapMaybe (uncurry $ snowdriftEventToFeedEntry
                        render
                        project_handle
                        user_map) events

    selectRep $ do
        provideRep $ atomFeed feed
        provideRep $ rssFeed feed
        provideRep $ defaultLayout $ do
            snowdriftDashTitle (projectName project) "Feed"
            $(widgetFile "project_feed")
            toWidget $(cassiusFile "templates/comment.cassius")

  where
    -- "event updated pledge to snowdrift event"
    eup2se :: Int64 -> Entity SharesPledged -> SnowdriftEvent
    eup2se old_shares (Entity shares_pledged_id shares_pledged) = EUpdatedPledge old_shares shares_pledged_id shares_pledged

    -- "event deleted pledge to snowdrift event"
    edp2se :: EventDeletedPledge -> SnowdriftEvent
    edp2se (EventDeletedPledge a b c d) = EDeletedPledge a b c d

--------------------------------------------------------------------------------
-- /invite

getInviteR :: Text -> Handler Html
getInviteR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . T.unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR project_handle <$> maybe_invite_code
    (invite_form, _) <- generateFormPost inviteForm

    outstanding_invites <- runDB $
        select $
        from $ \invite -> do
        where_ ( invite ^. InviteRedeemed ==. val False )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        return invite

    redeemed_invites <- runDB $
        select $
        from $ \invite -> do
        where_ ( invite ^. InviteRedeemed ==. val True )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        return invite

    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites
        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites
        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites
        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters

    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []

    let users = M.fromList $ map (entityKey &&& id) user_entities

    let format_user Nothing = "NULL"
        format_user (Just user_id) =
            let Entity _ user = fromMaybe (error "getInviteR: user_id not found in users map")
                                          (M.lookup user_id users)
             in fromMaybe (userIdent user) $ userName user

        format_inviter user_id =
            userDisplayName $ fromMaybe (error "getInviteR(#2): user_id not found in users map")
                                        (M.lookup user_id users)

    defaultLayoutNew "invite" $ do
        snowdriftDashTitle (projectName project) "Invite Roles"
        $(widgetFile "invite")

postInviteR :: Text -> Handler Html
postInviteR project_handle = do
    (user_id, Entity project_id _) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    invite <- liftIO randomIO

    ((result, _), _) <- runFormPost inviteForm
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = T.pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (T.pack $ show role)

        _ -> alertDanger "Error in submitting form."

    redirect $ InviteR project_handle

--------------------------------------------------------------------------------
-- /patrons

getProjectPatronsR :: Text -> Handler Html
getProjectPatronsR project_handle = do
    _ <- requireAuthId

    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges, user_payouts_map) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- select $ from $ \(pledge `InnerJoin` user) -> do
            on_ $ pledge ^. PledgeUser ==. user ^. UserId
            where_ $ pledge ^. PledgeProject ==. val project_id
                &&. pledge ^. PledgeFundedShares >. val 0
            orderBy [ desc (pledge ^. PledgeFundedShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        last_paydays <- case projectLastPayday project of
            Nothing -> return []
            Just last_payday -> select $ from $ \payday -> do
                where_ $ payday ^. PaydayId <=. val last_payday
                orderBy [ desc $ payday ^. PaydayId ]
                limit 2
                return payday

        user_payouts <- select $ from $ \(transaction `InnerJoin` user) -> do
            where_ $ transaction ^. TransactionPayday `in_` valList (map (Just . entityKey) last_paydays)
            on_ $ transaction ^. TransactionDebit ==. just (user ^. UserAccount)
            groupBy $ user ^. UserId
            return (user ^. UserId, count $ transaction ^. TransactionId)

        return (project, pledges, M.fromList $ map ((\(Value x :: Value UserId) -> x) *** (\(Value x :: Value Int) -> x)) user_payouts)

    defaultLayoutNew "project_patrons" $ do
        snowdriftTitle $ projectName project <> " Patrons"
        $(widgetFile "project_patrons")

--------------------------------------------------------------------------------
-- /pledge

getUpdatePledgeR :: Text -> Handler Html
getUpdatePledgeR project_handle = do
    _ <- requireAuthId
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    ((result, _), _) <- runFormGet $ pledgeForm project_id
    let dangerRedirect msg = do
            alertDanger msg
            redirect $ PHomeR project_handle
    case result of
        FormSuccess (SharesPurchaseOrder new_user_shares) -> do
            user_id <- requireAuthId

            (confirm_form, _) <-
                generateFormPost
                    (projectConfirmPledgeForm (Just new_user_shares))

            (mpledge
                , old_user_amount
                , new_user_amount
                , old_project_amount
                , new_project_amount
                , numPatrons
                ) <- runDB (Mech.potentialPledge user_id project_id new_user_shares)

            let new_user_mills = millMilray new_user_shares
            case mpledge of
                Just (Entity _ pledge) | pledgeShares pledge == new_user_shares -> do
                    alertWarning $ T.unwords
                        [ "Your pledge was already at"
                        , T.pack (show new_user_mills) <> "."
                        , "Thank you for your support!"
                        ]

                    redirect (PHomeR project_handle)

                _ -> do
                    let user_decrease    = old_user_amount - new_user_amount
                        user_increase    = new_user_amount - old_user_amount
                        project_decrease = old_project_amount - new_project_amount
                        project_increase = new_project_amount - old_project_amount
                        matching_drop   = project_decrease - user_decrease
                        matched_extra    = project_increase - new_user_amount
                        -- Standins added during mechanism split-out
                        old_user_mills = 0xdeadbeef :: Int64
                        old_user_shares = 0xbaff1ed :: Int64

                    defaultLayout $ do
                        snowdriftDashTitle
                            (projectName project)
                            "update pledge"
                        $(widgetFile "update_pledge")

        FormMissing -> dangerRedirect "Form missing."
        FormFailure errors ->
            dangerRedirect $ T.snoc (T.intercalate "; " errors) '.'

postUpdatePledgeR :: Text -> Handler Html
postUpdatePledgeR project_handle = do
    ((result, _), _) <- runFormPost $ projectConfirmPledgeForm Nothing
    isConfirmed <- maybe False (T.isPrefixOf "yes") <$> lookupPostParam "confirm"

    case result of
        FormSuccess (SharesPurchaseOrder shares) -> do
            when isConfirmed $ Mech.updateUserPledge project_handle shares
            redirect (PHomeR project_handle)
        _ -> do
            alertDanger "error occurred in form submission"
            redirect (UpdatePledgeR project_handle)


--------------------------------------------------------------------------------
-- /pledge-faux

getPledgeFauxR :: Text -> Handler Html
getPledgeFauxR handle = do
    Entity _ project <- runYDB (getBy404 (UniqueProjectHandle handle))
    alertWarning "Sorry, we're redoing the pledge system, so pledging isn't working right now."
    redirect $ PHomeR (projectHandle project)

--------------------------------------------------------------------------------
-- /transactions

getProjectTransactionsR :: Text -> Handler Html
getProjectTransactionsR project_handle = do
    (project, account, account_map, transaction_groups) <-
        runYDB (Mech.projectTransactions project_handle)

    let getOtherAccount transaction
            | transactionCredit transaction == Just (projectAccount project) = transactionDebit transaction
            | transactionDebit transaction == Just (projectAccount project) = transactionCredit transaction
            | otherwise = Nothing

    defaultLayoutNew "project_transactions" $ do
        snowdriftTitle $ projectName project <> " Transactions"
        $(widgetFile "project_transactions")

--------------------------------------------------------------------------------
-- /watch, /unwatch

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "Watching "
postUnwatchProjectR = watchOrUnwatchProject userUnwatchProjectDB "No longer watching "

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject action msg project_id = do
    user_id <- requireAuthId
    project <- runYDB $ do
        action user_id project_id
        get404 project_id
    alertSuccess (msg <> projectName project <> ".")
    redirect $ PHomeR (projectHandle project)
