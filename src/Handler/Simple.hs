{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Simple where

import Import

import Network.HTTP.Types.Status (movedPermanently301)

import Handler.TH
import Handler.Utils
import Widgets.Doc

getIntroHomeR,
    getShareableR,
    getMatchingR,
    getSustainabilityR,
    getDemocracyR,
    getAboutR,
    getContactR,
    getPSignupR,
    getTeamR,
    getPressR,
    getSponsorsR
    :: Handler Html
getIntroHomeR      = $(simpleHandler "intro/home" "Intro")
getShareableR      = $(simpleHandler "intro/shareable-works" "Shareable Works")
getMatchingR       = $(simpleHandler "intro/matching" "Matching Pledges")
getSustainabilityR = $(simpleHandler "intro/sustainability" "Sustainable Funding")
getDemocracyR      = $(simpleHandler "intro/democracy" "Democracy")
getAboutR          = $(simpleHandler "about" "About")
getPSignupR        = $(simpleHandler "project-signup" "Project Signup")
getContactR        = $(simpleHandler "contact" "Contact")
getTeamR           = $(simpleHandler "team" "Team")
getPressR          = $(simpleHandler "press" "Press")
getSponsorsR       = $(simpleHandler "sponsors" "Sponsors")

-- * TODO: Convert these.
getTermsR,
    getPrivacyR,
    getTrademarksR
    :: Handler Html
getTermsR = defaultLayoutNew "terms-of-use" $ do
    snowdriftTitle "Terms of Use"
    renderDoc "Terms of Use"
getPrivacyR = defaultLayoutNew "privacy" $ do
    snowdriftTitle "Privacy Policy"
    renderDoc "Privacy Policy"
getTrademarksR = defaultLayoutNew "trademarks" $ do
    snowdriftTitle "Trademarks"
    renderDoc "Trademarks"

-- | Permanent redirects for legacy urls that may still be referenced
-- outside of the type-safe project
getLegacyTouR,
    getLegacyPrivR
    :: Handler Html
getLegacyTouR  = redirectWith movedPermanently301 TermsR
getLegacyPrivR = redirectWith movedPermanently301 PrivacyR
