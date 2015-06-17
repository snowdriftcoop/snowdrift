{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Model.TH

import Model.Comment.Internal      (FlagReason, Visibility)
import Model.Currency              (Milray)
import Model.Discussion.TH         (mkDiscussionTypes)
import Model.Established.Internal  (Established(..))
import Model.Language
import Model.License.Internal
       (LicenseName, LicenseType, LicenseProjectType, LicenseText,
        LicenseWebsite)
import Model.Markdown.Diff (MarkdownDiff)
import Model.Notification.Internal
       (UserNotificationType, UserNotificationDelivery,
        ProjectNotificationType, ProjectNotificationDelivery)
import Model.Permission.Internal (PermissionLevel)
import Model.Project.Signup
       (ProjectSignupName, ProjectSignupWebsite, ProjectSignupHandle,
        ProjectSignupStartDate, ProjectSignupLocation,
        ProjectSignupApplicantRole, ProjectSignupMission,
        ProjectSignupGoals, ProjectSignupFundsUse,
        ProjectSignupAdditionalInfo, ProjectSignupLicenseComment)
import Model.Project.Signup.Internal
       (ProjectSignupCategory, ProjectSignupCategoryComment,
        ProjectSignupLegalStatus, ProjectSignupLegalStatusComment,
        ProjectSignupCoopStatus)
import Model.Role.Internal         (Role)
import Model.Settings.Internal     (UserSettingName)
import Model.ViewType.Internal     (ViewType)

import Control.Exception           (Exception)
import Data.ByteString             (ByteString)
import Data.Int                    (Int64)
import Data.Ord
import Data.Text                   (Text)
import Data.Time.Clock             (UTCTime)
import Data.Typeable               (Typeable)
import Database.Persist.Quasi
-- Yesod excludes the Prelude, and the table schemata use Prelude types,
-- like Int and Bool.
import Prelude
import Yesod
import Yesod.Auth.HashDB           (HashDBUser (..))
import Yesod.Markdown              (Markdown)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- 
-- http://www.yesodweb.com/book/persistent/
-- 
-- Automatic migrations use up memory. See
-- <https://github.com/yesodweb/persistent/issues/403>. They aren't
-- really great practice either. For now, they're commented out. In the
-- future, we'll likely just delete them.
share [ mkPersist sqlSettings
      -- , mkMigrate "migrateAll"
      , mkDeleteCascade sqlSettings
      , mkDiscussionTypes
      , mkReferences "Comment"
      ]
      $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = userHash
    userPasswordSalt = userSalt
    setSaltAndPasswordHash salt hash user = user { userHash = Just hash, userSalt = Just salt }

data DBException = DBException deriving (Typeable, Show)

instance Exception DBException where

instance Ord Project where
    compare = comparing projectName

deriving instance Eq License
deriving instance Show License
deriving instance Read License

data ProjectSignupLicense
  = ProjectSignupLicense License
  | OtherProjectSignupLicense
  deriving (Eq,Show,Read)
derivePersistField "ProjectSignupLicense"

deriving instance Show ProjectSignup

deriving instance Eq UserNotification
deriving instance Eq ProjectNotification
