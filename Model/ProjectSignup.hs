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

newProjectSignupStatus :: ProjectSignupStatus
newProjectSignupStatus = InReview

