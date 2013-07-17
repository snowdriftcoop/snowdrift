
module Model.Role 
    ( Role (..)
    , roleCanInvite
    , roleDefaultTarget
    , roleLabel
    , roleAbbrev
    , roleField
    ) where

import Import

import Model.Role.Internal
--import Yesod.Routes.Class (Route)


roleCanInvite :: Role -> Role -> Bool
roleCanInvite CommitteeMember CommitteeMember = False
roleCanInvite Editor GeneralPublic = True
roleCanInvite Editor _ = False
roleCanInvite a b = a >= b

roleDefaultTarget :: Role -> Route App
roleDefaultTarget Public = error "Public should never be a user's role"
roleDefaultTarget Uninvited = HomeR
roleDefaultTarget GeneralPublic = WikiR "about"
roleDefaultTarget CommitteeCandidate = WikiR "joincommittee"
roleDefaultTarget CommitteeMember = WikiR "committee"
roleDefaultTarget Admin = WikiR "about"
roleDefaultTarget Editor = WikiR "about"

roleLabel :: Role -> Text
roleLabel Public = "Public"
roleLabel Uninvited = "Uninvited"
roleLabel GeneralPublic = "General Interest"
roleLabel CommitteeCandidate = "Steering Committee Candidate"
roleLabel CommitteeMember = "Steering Committee Member"
roleLabel Editor = "Editor"
roleLabel Admin = "Admin"

roleAbbrev :: Role -> Text
roleAbbrev Public = "P"
roleAbbrev Uninvited = "U"
roleAbbrev GeneralPublic = "G"
roleAbbrev CommitteeCandidate = "C"
roleAbbrev CommitteeMember = "M"
roleAbbrev Editor = "E"
roleAbbrev Admin = "A"

roleField :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO) => Role -> Field m Role
roleField role =
    let roles = filter (roleCanInvite role) [GeneralPublic .. Admin]
     in case roles of
            [] -> error "role cannot issue invitations"
            [GeneralPublic] -> hiddenField
            _ -> radioFieldList $ map (\ r -> (roleLabel r, r)) roles

