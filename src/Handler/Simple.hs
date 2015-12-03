{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Simple where

import Import

import Handler.TH
import Handler.Utils
import Widgets.Doc

getHomeR,
    getIntroHomeR,
    getShareableR,
    getMatchingR,
    getSustainabilityR,
    getDemocracyR,
    getAboutR,
    getTermsR,
    getPrivacyR,
    getContactR,
    getPSignupR
        :: Handler Html

getHomeR           = $(simpleHandler "homepage" "Free the Commons")
getIntroHomeR      = $(simpleHandler "intro/home" "Intro")
getShareableR      = $(simpleHandler "intro/shareable-works" "Shareable Works")
getMatchingR       = $(simpleHandler "intro/matching" "Matching Pledges")
getSustainabilityR = $(simpleHandler "intro/sustainability" "Sustainable Funding")
getDemocracyR      = $(simpleHandler "intro/democracy" "Democracy")
getAboutR          = $(simpleHandler "about" "About")
getPSignupR        = $(simpleHandler "project-signup" "Project Signup")
getContactR        = $(simpleHandler "contact" "Contact")
-- * TODO: Convert these.
getTermsR = defaultLayoutNew "terms-of-use" $ do
    snowdriftTitle "Terms of Use"
    renderDoc "Terms of Use"
getPrivacyR = defaultLayoutNew "privacy" $ do
    snowdriftTitle "Privacy Policy"
    renderDoc "Privacy Policy"
