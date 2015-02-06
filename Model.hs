{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, StandaloneDeriving #-}

module Model where

import Model.TH

import Model.Comment.Internal      (FlagReason, Visibility)
import Model.Currency              (Milray)
import Model.Discussion.TH         (mkDiscussionTypes)
import Model.Established.Internal  (Established(..))
import Model.Language
import Model.License.Internal      (LicenseName, LicenseType, LicenseProjectType, LicenseText, LicenseWebsite)
import Model.Markdown.Diff         (MarkdownDiff)
import Model.Notification.Internal (NotificationType, NotificationDelivery)
import Model.Permission.Internal   (PermissionLevel)
import Model.Project.Signup
    ( ProjectSignupName, ProjectSignupWebsite, ProjectSignupHandle
    , ProjectSignupStartDate, ProjectSignupLocation, ProjectSignupApplicantRole
    , ProjectSignupMission, ProjectSignupGoals, ProjectSignupFundsUse
    , ProjectSignupAdditionalInfo )
import Model.Project.Signup.Internal
    (ProjectSignupCategory , ProjectSignupLegalStatus)
import Model.Role.Internal         (Role)
import Model.Settings.Internal     (UserSettingName)
import Model.ViewType.Internal     (ViewType)

import Control.Exception           (Exception)
import Data.Int                    (Int64)
import Data.Function               (on)
import Data.Text                   (Text)
import Data.ByteString             (ByteString)
import Data.Time.Clock             (UTCTime)
import Data.Typeable               (Typeable)
import Database.Persist.Quasi
import Prelude
import Yesod
import Yesod.Auth.HashDB           (HashDBUser (..))
import Yesod.Markdown              (Markdown)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
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
    compare = compare `on` projectName

deriving instance Eq License
deriving instance Show License
deriving instance Read License

data ProjectSignupLicense = ProjectSignupLicense License
                          | OtherProjectSignupLicense Text
                          deriving (Eq, Show, Read)
derivePersistField "ProjectSignupLicense"

deriving instance Show ProjectSignup
