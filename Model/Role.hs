
module Model.Role 
    ( Role (..)
    , roleCanInvite
    , roleLabel
    , roleAbbrev
    , roleField
    ) where

import Import

import Model.Role.Internal


roleCanInvite :: Role -> Role -> Bool
roleCanInvite CommitteeMember CommitteeMember = False
roleCanInvite Editor GeneralPublic = True
roleCanInvite Editor _ = False
roleCanInvite a b = a >= b

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

