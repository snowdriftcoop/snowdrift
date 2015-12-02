{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Simple where

import Import

import Handler.TH
import Handler.Utils
import Widgets.Doc

getHomeR,
    getIntroHomeR,
    getSharingR,
    getMatchingR,
    getSustainabilityR,
    getDemocracyR,
    getAboutR,
    getTermsR,
    getContactR
        :: Handler Html

getHomeR           = $(simpleHandler "homepage" "Free the Commons")
getIntroHomeR      = $(simpleHandler "intro/home" "Some Title Here")
getSharingR        = $(simpleHandler "intro/sharing" "Sharing")
getMatchingR       = $(simpleHandler "intro/matching" "Matching")
getSustainabilityR = $(simpleHandler "intro/sustainability" "Sustainability")
getDemocracyR      = $(simpleHandler "intro/democracy" "Democracy")
getAboutR          = $(simpleHandler "about" "About")
getTermsR = defaultLayoutNew "terms-of-use" $ do
    snowdriftTitle "Terms of Use"
    renderDoc "Terms of Use"
getContactR        = $(simpleHandler "contact" "Contact")
