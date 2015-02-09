{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Project.Signup where

import Prelude             (Int, Show, Eq, Read)
import Data.Hourglass      (Month (..))
import Data.Text           (Text)
import Database.Persist.TH
import Yesod.Form.Fields   (Textarea)

newtype ProjectSignupName    = ProjectSignupName    Text deriving (Show, Read)
derivePersistField "ProjectSignupName"

newtype ProjectSignupWebsite = ProjectSignupWebsite Text deriving (Show, Read)
derivePersistField "ProjectSignupWebsite"

newtype ProjectSignupHandle  = ProjectSignupHandle  Text deriving (Show, Read)
derivePersistField "ProjectSignupHandle"

newtype Year = Year Int deriving (Show, Eq)

deriving instance Read Year
deriving instance Read Month

newtype ProjectSignupStartDate = ProjectSignupStartDate (Year, Month) deriving (Show, Read)
derivePersistField "ProjectSignupStartDate"

data ProjectSignupLocation = ProjectSignupLocation Text deriving (Show, Read)
derivePersistField "ProjectSignupLocation"

newtype ProjectSignupApplicantRole  = ProjectSignupApplicantRole Text deriving (Show, Read)
derivePersistField "ProjectSignupApplicantRole"

newtype ProjectSignupMission        = ProjectSignupMission        Textarea deriving (Show, Read)
derivePersistField "ProjectSignupMission"
newtype ProjectSignupGoals          = ProjectSignupGoals          Textarea deriving (Show, Read)
derivePersistField "ProjectSignupGoals"
newtype ProjectSignupFundsUse       = ProjectSignupFundsUse       Textarea deriving (Show, Read)
derivePersistField "ProjectSignupFundsUse"
newtype ProjectSignupAdditionalInfo = ProjectSignupAdditionalInfo Textarea deriving (Show, Read)
derivePersistField "ProjectSignupAdditionalInfo"

newtype ProjectSignupLicenseComment = ProjectSignupLicenseComment Text deriving (Eq, Show, Read)
derivePersistField "ProjectSignupLicenseComment"
