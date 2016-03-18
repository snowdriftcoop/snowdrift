module Model.Project
    ( ProjectSummary(..)
    , UpdateProject(..)
    , fetchPublicProjectsDB
    , fetchProjectDB
    , fetchProjectModeratorsDB
    , fetchProjectTeamMembersDB
    , fetchProjectVolunteerApplicationsDB
    , fetchProjectPledgesDB
    , projectNameWidget
    , summarizeProject
    -- * Balancing/deactivating pledges
    ) where

import Import

import Control.Monad.Trans.Resource (MonadThrow)

import Model.Count
import WrappedValues

--------------------------------------------------------------------------------
-- Types

newtype UserCount = UserCount Int64 deriving Count
newtype ShareCount = ShareCount Int64 deriving Count
newtype DiscussionCount = DiscussionCount Int64 deriving Count
newtype TicketCount = TicketCount Int64 deriving Count

data ProjectSummary = ProjectSummary
    { summaryName            :: Text
    , summaryProjectHandle   :: Text
    }

data UpdateProject = UpdateProject
    { updateProjectName        :: Text
    , updateProjectBlurb       :: Text
    , updateProjectDescription :: Markdown
    , updateProjectTags        :: [Text]
    , updateProjectGithubRepo  :: Maybe Text
    , updateProjectLogo        :: Maybe Text
    } deriving Show

--------------------------------------------------------------------------------
-- Database actions

fetchPublicProjectsDB :: DB [Entity Project]
fetchPublicProjectsDB = select $ from $ \p -> do
    where_ $ p ^. ProjectPublic
    return p

fetchProjectDB :: ProjectId -> DB [Entity Project]
fetchProjectDB project_id =
    select $ from $ \p -> do
        where_ $ p ^. ProjectId ==. val project_id
        return p

summarizeProject :: Entity Project
                 -> ProjectSummary
summarizeProject project =
    ProjectSummary
        (projectName $ entityVal project)
        (projectHandle $ entityVal project)

fetchProjectPledgesDB :: ( MonadThrow m
                         , MonadIO m
                         , MonadBaseControl IO m
                         , MonadLogger m
                         , MonadResource m)
                      => ProjectId
                      -> SqlPersistT m [Entity Pledge]
fetchProjectPledgesDB project_id = do
    pledges <- select $ from $ \pledge -> do
        where_
            (pledge ^. PledgeProject ==. val project_id
             &&. pledge ^. PledgeFundedShares >. val 0)
        return pledge

    return pledges

projectNameWidget :: ProjectId -> Widget
projectNameWidget project_id = do
    maybe_project <- handlerToWidget $ runDB $ get project_id
    case maybe_project of
        Nothing -> [whamlet| (unknown project) |]
        Just project -> [whamlet|
                            <a href=@{PHomeR (projectHandle project)}>
                              #{projectName project}
                        |]

-- | Fetch this Project's team members.
fetchProjectTeamMembersDB :: ProjectId -> DB [UserId]
fetchProjectTeamMembersDB = fetchProjectRoleDB TeamMember

fetchProjectModeratorsDB :: ProjectId -> DB [UserId]
fetchProjectModeratorsDB = fetchProjectRoleDB Moderator

-- | Abstract fetching Project Admins, TeamMembers, etc. Not exported.
fetchProjectRoleDB :: Role -> ProjectId -> DB [UserId]
fetchProjectRoleDB role project_id = fmap unwrapValues $
    select $
    from $ \pur -> do
    where_ $
        pur ^. ProjectUserRoleProject ==. val project_id &&.
        pur ^. ProjectUserRoleRole    ==. val role
    return (pur ^. ProjectUserRoleUser)
  --
-- | Fetch all Project VolunteerApplications.
fetchProjectVolunteerApplicationsDB
    :: ProjectId
    -> DB [Entity VolunteerApplication]
fetchProjectVolunteerApplicationsDB project_id =
    select $
    from $ \va -> do
    where_ (va ^. VolunteerApplicationProject ==. val project_id)
    orderBy [desc (va ^. VolunteerApplicationCreatedTs)]
    return va
