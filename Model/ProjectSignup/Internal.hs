module Model.ProjectSignup.Internal where
import Prelude
import Database.Persist.TH

data ProjectType = CreativeWriting | Education | Games | Hardware | Journalism | Music | Research | Software | Video | Art | OtherProjectType deriving (Read, Show, Eq, Ord, Bounded, Enum)
derivePersistField "ProjectType"

data ProjectSignupStatus = InReview | Approved | Denied deriving (Read, Show, Eq, Ord, Bounded, Enum)
derivePersistField "ProjectSignupStatus"
