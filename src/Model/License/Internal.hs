module Model.License.Internal where

import Prelude (Show, Read, Eq)

import Data.Text (Text)
import Database.Persist.TH

import Model.Project.Signup.Internal

newtype LicenseName = LicenseName { unLicenseName :: Text }
    deriving (Show, Read, Eq)
derivePersistField "LicenseName"

data LicenseType = CopyLeft | CopyFree | OtherLicenseType Text
    deriving (Show, Read, Eq)
derivePersistField "LicenseType"

newtype LicenseProjectType = LicenseProjectType ProjectSignupCategory
    deriving (Show, Read, Eq)
derivePersistField "LicenseProjectType"

newtype LicenseText = LicenseText Text
    deriving (Show, Read, Eq)
derivePersistField "LicenseText"

newtype LicenseWebsite = LicenseWebsite Text
    deriving (Show, Read, Eq)
derivePersistField "LicenseWebsite"
