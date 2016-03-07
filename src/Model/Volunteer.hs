module Model.Volunteer where

import Import

-- | Record a volunteer application.
insertVolunteerApplicationDB :: ProjectId -> VolunteerApplication -> [InterestId] -> DB ()
insertVolunteerApplicationDB _project_id application interest_ids = do
    application_id <- insert application
    forM_ interest_ids $ \interest_id ->
        insert (VolunteerInterest application_id interest_id)
