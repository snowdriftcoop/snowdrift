
module Model.Role 
    ( Role (..)
    , roleCanInvite
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

roleLabel :: Role -> Text
roleLabel Uninvited = "Uninvited"
roleLabel GeneralPublic = "General Interest"
roleLabel TeamMember = "Team Member"
roleLabel CommitteeCandidate = "Steering Committee Candidate"
roleLabel CommitteeMember = "Steering Committee Member"
roleLabel BoardMember = "Board Member"
roleLabel Editor = "Editor"
roleLabel Admin = "Admin"

roleAbbrev :: Role -> Text
roleAbbrev Uninvited = "U"
roleAbbrev GeneralPublic = "G"
roleAbbrev TeamMember = "T"
roleAbbrev CommitteeCandidate = "C"
roleAbbrev CommitteeMember = "M"
roleAbbrev BoardMember = "B"
roleAbbrev Editor = "E"
roleAbbrev Admin = "A"

roleField :: RenderMessage master FormMessage => Role -> Field sub master Role
roleField role =
    let roles = filter (roleCanInvite role) [GeneralPublic .. Admin]
     in case roles of
            [] -> error "role cannot issue invitations"
            [GeneralPublic] -> hiddenField
            _ -> radioFieldList $ map (\ r -> (roleLabel r, r)) roles

