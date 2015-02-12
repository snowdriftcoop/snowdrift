{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Project.Signup where

import Prelude             (Int, Show, Eq, Read)
import Data.Hourglass      (Month (..))
import Data.Text           (Text)
import Database.Persist.TH
import Yesod.Markdown      (Markdown)

import Model.Project.Signup.TH

newtype ProjectSignupName    = ProjectSignupName    Text deriving (Show, Read)
derivePersistFieldText ''ProjectSignupName
newtype ProjectSignupWebsite = ProjectSignupWebsite Text deriving (Show, Read)
derivePersistFieldText ''ProjectSignupWebsite
newtype ProjectSignupHandle  = ProjectSignupHandle  Text deriving (Show, Read)
derivePersistFieldText ''ProjectSignupHandle

newtype Year = Year Int deriving (Show, Eq)

deriving instance Read Year
deriving instance Read Month

newtype ProjectSignupStartDate = ProjectSignupStartDate (Year, Month) deriving (Show, Read)
derivePersistField "ProjectSignupStartDate"

newtype ProjectSignupLocation = ProjectSignupLocation Text deriving (Show, Read)
derivePersistFieldText ''ProjectSignupLocation

newtype ProjectSignupApplicantRole  = ProjectSignupApplicantRole Text deriving (Show, Read)
derivePersistFieldText ''ProjectSignupApplicantRole

newtype ProjectSignupMission        = ProjectSignupMission        Markdown deriving (Show, Read)
derivePersistFieldMarkdown ''ProjectSignupMission
newtype ProjectSignupGoals          = ProjectSignupGoals          Markdown deriving (Show, Read)
derivePersistFieldMarkdown ''ProjectSignupGoals
newtype ProjectSignupFundsUse       = ProjectSignupFundsUse       Markdown deriving (Show, Read)
derivePersistFieldMarkdown ''ProjectSignupFundsUse
newtype ProjectSignupAdditionalInfo = ProjectSignupAdditionalInfo Markdown deriving (Show, Read)
derivePersistFieldMarkdown ''ProjectSignupAdditionalInfo

newtype ProjectSignupLicenseComment = ProjectSignupLicenseComment Text deriving (Eq, Show, Read)
derivePersistFieldText ''ProjectSignupLicenseComment
