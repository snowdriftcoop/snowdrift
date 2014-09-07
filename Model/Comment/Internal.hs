module Model.Comment.Internal where

import Prelude

import Database.Persist.TH
import Data.Text           (Text)

import Yesod.Markdown      (Markdown)

-- VisPublic = visible to all | VisPrivate = visible to topic-poster and
-- those with provenance over the discussion (e.g. project team for project
-- discussion ) | VisInternal = visible only to those with provenance over
-- the discussion
data Visibility = VisPublic | VisPrivate | VisInternal deriving (Read, Show, Eq)
derivePersistField "Visibility"

newtype NewClosure = NewClosure Markdown
data NewComment = NewComment Markdown Visibility

data FlagReason
    = FlagPersonalAttack
    | FlagUnconstructiveCriticism
    | FlagCondescension
    | FlagDefensiveness
    | FlagSpamming
    | FlagPrivacyViolation
    | FlagHateSpeech
    deriving (Bounded, Enum, Eq, Read, Show)
derivePersistField "FlagReason"

descFlagReason :: FlagReason -> Text
descFlagReason FlagPersonalAttack          = "Personal attack"
descFlagReason FlagUnconstructiveCriticism = "Unconstructive criticism"
descFlagReason FlagCondescension           = "Condescension"
descFlagReason FlagDefensiveness           = "Defensiveness"
descFlagReason FlagSpamming                = "Spamming"
descFlagReason FlagPrivacyViolation        = "Privacy violation"
descFlagReason FlagHateSpeech              = "Hate speech"
