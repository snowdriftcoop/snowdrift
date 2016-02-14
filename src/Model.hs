{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Model where

import Prelude

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi
import Yesod
import Yesod.Auth.HashDB (HashDBUser (..))
import Yesod.Markdown (Markdown)

import Model.Comment.Internal (FlagReason,Visibility)
import Model.Currency (Milray)
import Model.Discussion.TH (mkDiscussionTypes)
import Model.Established.Internal (Established(..))
import Model.Language
import Model.License.Internal
            (LicenseName
            ,LicenseType
            ,LicenseProjectType
            ,LicenseText
            ,LicenseWebsite)
import Model.Markdown.Diff (MarkdownDiff)
import Model.Notification.Internal
            (UserNotificationType
            ,UserNotificationDelivery
            ,ProjectNotificationType
            ,ProjectNotificationDelivery)
import Model.Permission.Internal (PermissionLevel)
import Model.Project.Signup
            (ProjectSignupName
            ,ProjectSignupWebsite
            ,ProjectSignupHandle
            ,ProjectSignupStartDate
            ,ProjectSignupLocation
            ,ProjectSignupApplicantRole
            ,ProjectSignupMission
            ,ProjectSignupGoals
            ,ProjectSignupFundsUse
            ,ProjectSignupAdditionalInfo
            ,ProjectSignupLicenseComment)
import Model.Project.Signup.Internal
            (ProjectSignupCategory
            ,ProjectSignupCategoryComment
            ,ProjectSignupLegalStatus
            ,ProjectSignupLegalStatusComment
            ,ProjectSignupCoopStatus)
import Model.Role.Internal (Role)
import Model.Settings.Internal (UserSettingName)
import Model.TH
import Model.ViewType.Internal (ViewType)

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

-- we usually use "passphrase" but these uses of "password" come from Yesod
instance HashDBUser User where
    userPasswordHash = userHash
    userPasswordSalt = userSalt
    setSaltAndPasswordHash salt hash user = user { userHash = Just hash, userSalt = Just salt }

data DBException = DBException deriving (Typeable, Show)

instance Exception DBException where

instance Ord Project where
    compare = compare `on` projectName

data ProjectSignupLicense = ProjectSignupLicense License
                          | OtherProjectSignupLicense
                          deriving (Eq, Show, Read)
derivePersistField "ProjectSignupLicense"
