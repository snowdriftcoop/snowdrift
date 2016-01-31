module Model.Volunteer where

import Import

import Control.Monad.Trans.Writer.Strict (tell)

-- | Record a volunteer application.
insertVolunteerApplicationDB :: ProjectId -> VolunteerApplication -> [InterestId] -> SDB ()
insertVolunteerApplicationDB project_id application interest_ids = do
    application_id <- lift (insert application)
    forM_ interest_ids $ \interest_id ->
        lift (insert (VolunteerInterest application_id interest_id))

    now <- liftIO getCurrentTime
    tell [EVolunteerApp now (volunteerApplicationUser application) project_id application_id]
