module Model.ProjectSignup where

import Model.ProjectSignup.Internal
import Import
import Data.List as L

getProjectTypeLabel :: ProjectType -> Text
getProjectTypeLabel pt = case pt of
                            Art -> "Visual Art"
                            CreativeWriting -> "Creative Writing"
                            Education -> "Educational"
                            Games -> "Games"
                            Hardware -> "Hardware Designs"
                            Journalism -> "Journalism"
                            Music -> "Music"
                            Software -> "Software"
                            Research -> "Research"
                            Video -> "Video"
                            OtherProjectType -> "Other"

getProjectTypes :: [(Text, ProjectType)]
getProjectTypes = L.map (getProjectTypeLabel &&& id) ([minBound .. maxBound] :: [ProjectType])

getProjectSignupStatusLabel :: ProjectSignupStatus -> Text
getProjectSignupStatusLabel pss = case pss of
                                InReview -> "In Review"
                                Approved -> "Approved"
                                Denied -> "Denied"

getProjectSignupStatuses :: [(Text, ProjectSignupStatus)]
getProjectSignupStatuses = L.map (getProjectSignupStatusLabel &&& id) ([minBound .. maxBound] :: [ProjectSignupStatus])

newProjectSignupStatus :: ProjectSignupStatus
newProjectSignupStatus = InReview

