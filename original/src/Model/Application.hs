module Model.Application where

import Import

fetchApplicationVolunteerInterestsDB :: VolunteerApplicationId -> DB [Text]
fetchApplicationVolunteerInterestsDB application_id = fmap (map unValue) $
    select $
    from $ \(vi `InnerJoin` i) -> do
    on_ (i ^. InterestId ==. vi ^. VolunteerInterestInterest)
    where_ (vi ^. VolunteerInterestVolunteer ==. val application_id)
    return (i ^. InterestDescription)
