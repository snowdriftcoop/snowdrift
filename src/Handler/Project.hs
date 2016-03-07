{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import System.Random (randomIO)
import Text.Printf
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
import Model.User
import View.Project
import View.Time

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
getProjectFeedR _project_handle = selectRep $ provideRep $ defaultLayout $ return ()

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
getUpdatePledgeR _project_handle = return ""

postUpdatePledgeR :: Text -> Handler Html
postUpdatePledgeR _project_handle = return ""


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
getProjectTransactionsR _project_handle = return ""

--------------------------------------------------------------------------------
-- /watch, /unwatch

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = const (return ())
postUnwatchProjectR = const (return ())

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject _action _msg _project_id = return ()
