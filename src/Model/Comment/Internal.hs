module Model.Comment.Internal where

import Prelude

import Database.Persist.TH
import Data.Text           (Text)

import Model.Language

import Yesod.Markdown      (Markdown)

-- VisPublic = visible to all | VisPrivate = visible to topic-poster and
-- those with provenance over the discussion (e.g. project team for project
-- discussion ) | VisInternal = visible only to those with provenance over
-- the discussion
data Visibility
    = VisInternal
    | VisPrivate
    | VisPublic
    deriving (Read, Show, Eq, Ord)
derivePersistField "Visibility"

newtype NewClosure = NewClosure Markdown
data NewComment = NewComment Markdown Visibility Language
data EditComment = EditComment Markdown Language

data FlagReason
    = FlagViolateAGF
    | FlagPersonalAttack
    | FlagUnconstructiveCriticism
    | FlagCondescension
    | FlagDefensiveness
    | FlagSpamming
    | FlagPrivacyViolation
    | FlagMisrepresentation
    | FlagHateSpeech
    deriving (Bounded, Enum, Eq, Read, Show)
derivePersistField "FlagReason"

descFlagReason :: FlagReason -> Text
descFlagReason FlagViolateAGF              = "Assumption of bad faith"
descFlagReason FlagPersonalAttack          = "Personal attack"
descFlagReason FlagUnconstructiveCriticism = "Unconstructive criticism"
descFlagReason FlagCondescension           = "Condescension"
descFlagReason FlagDefensiveness           = "Defensiveness"
descFlagReason FlagSpamming                = "Spamming"
descFlagReason FlagPrivacyViolation        = "Privacy violation"
descFlagReason FlagMisrepresentation       = "Misrepresentation"
descFlagReason FlagHateSpeech              = "Hate speech"

newtype TicketName = TicketName { unTicketName :: Text } deriving Eq
newtype TagName    = TagName    { unTagName    :: Text } deriving Eq
