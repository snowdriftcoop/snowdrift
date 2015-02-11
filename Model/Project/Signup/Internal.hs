module Model.Project.Signup.Internal where

import Prelude (Show, Read, Eq, Enum, Bounded)

import Data.Text (Text)
import Database.Persist.TH

data ProjectSignupCategory = CreativeWriting
                           | Education
                           | Games
                           | HardwareDesign
                           | Journalism
                           | Music
                           | Research
                           | Software
                           | Video
                           | VisualArt
                           deriving (Eq, Show, Read, Enum, Bounded)
derivePersistField "ProjectSignupCategory"

newtype ProjectSignupCategoryComment = ProjectSignupCategoryComment Text deriving (Eq, Show, Read)
derivePersistField "ProjectSignupCategoryComment"

data ProjectSignupLegalStatus = NonProfitTaxDeductible
                              | NonProfitNotTaxDeductible
                              | TradeOrganization
                              | ForProfitSocial
                              | ForProfitTraditional
                              | Unincorporated
                              deriving (Eq, Show, Read, Enum, Bounded)
derivePersistField "ProjectSignupLegalStatus"

newtype ProjectSignupLegalStatusComment = ProjectSignupLegalStatusComment Text deriving (Eq, Show, Read)
derivePersistField "ProjectSignupLegalStatusComment"
