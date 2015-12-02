-- | This module should not live beyond January 2016. PLEASE PLEASE SHOOT
-- IT DEAD AFTER THAT DATE.
--
-- This module is just a placeholder before fleshing out the modules needed
-- for the alpha sprint
module Handler.NewDesign where

import Import

import Handler.TH
import Handler.Utils

getSearchR,
    getSignupFormR,
    getUSignupR,
    postUSignupR,
    getUTransactionsR,
    getUNoticesR,
    getUPledgesR,
    getUMembershipR,
    getUEditR
    :: Handler Html

getSearchR        = $(simpleHandler "search" "Search")
getSignupFormR    = $(simpleHandler "project/signup" "Project Signup")
getUSignupR       = $(simpleHandler "signup" "Signup")
postUSignupR      = $(simpleHandler "post-signup" "Signup")
getUTransactionsR = $(simpleHandler "transactions" "Transactions")
getUNoticesR      = $(simpleHandler "notices" "Notices")
getUPledgesR      = $(simpleHandler "pledges" "Pledges")
getUMembershipR   = $(simpleHandler "memberships" "Project Memberships")
getUEditR         = $(simpleHandler "edit-profile" "Edit Profile")

getPHomeR,
    getPUpdatesR,
    getPTransactionsR
    :: Text -> Handler Html

getPHomeR handle =
    defaultLayoutNew "project/home" $ do
        snowdriftTitle handle
        $(widgetFile "project/home")
getPUpdatesR handle =
    defaultLayoutNew "project/updates" $ do
        snowdriftTitle (handle <> ": Updates")
        $(widgetFile "project/updates")
getPTransactionsR handle =
    defaultLayoutNew "project/transactions" $ do
        snowdriftTitle (handle <> ": Transactions")
        $(widgetFile "project/transactions")
