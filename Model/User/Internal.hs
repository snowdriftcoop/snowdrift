module Model.User.Internal where

import Prelude

import Database.Persist.TH
import Data.Text (Text)

data MessagePreference
    = MessageOnReply
    | MessageOnNewProject
    | MessageOnNewPledger
    | MessageOnNewPage
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "MessagePreference"

showMessagePreference :: MessagePreference -> Text
showMessagePreference MessageOnReply      = "Replies to my comments"
showMessagePreference MessageOnNewProject = "New project sign-ups"
showMessagePreference MessageOnNewPledger = "New pledgers (to watched projects)"
showMessagePreference MessageOnNewPage    = "New Wiki pages (on watched projects)"
