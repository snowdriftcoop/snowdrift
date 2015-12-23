module Handler.Simple where

import Import

import Network.HTTP.Types.Status (movedPermanently301)

import Dev
import Handler.TH
import Handler.Utils
import Widgets.Doc

getIntroR,
    getFloR,
    getNetworkEffectR,
    getSustainableFundingR,
    getCoOpR,
    getAboutHomeR,
    getContactR,
    getPSignupR,
    getTeamR,
    getPressR,
    getSponsorsR,
    getDonateR,
    getMerchandiseR
    getAssetsR
    :: Handler Html
getIntroR              = $(simpleHandler "how-it-works/intro" "Intro")
getFloR                = $(simpleHandler "how-it-works/flo" "FLO")
getNetworkEffectR      = $(simpleHandler "how-it-works/network-effect"
                                         "Network Effect")
getSustainableFundingR = $(simpleHandler "how-it-works/sustainable-funding"
                                         "Sustainable Funding")
getCoOpR               = $(simpleHandler "how-it-works/co-op" "Co-op")
getPSignupR            = $(simpleHandler "project-signup" "Project Signup")
getAboutHomeR          = $(simpleHandler "about/old-homepage" "About")
getContactR            = $(simpleHandler "contact" "Contact")
getTeamR               = $(simpleHandler "team" "Team")
getPressR              = $(simpleHandler "press" "Press")
getSponsorsR           = $(simpleHandler "sponsors" "Sponsors")
getDonateR             = $(simpleHandler "donate" "Donate")
getMerchandiseR        = $(simpleHandler "merchandise" "Merchandise")
getAssetsR             = $(simpleHandler "assets" "Assets")

-- * TODO: Convert these.
getTermsR,
    getPrivacyR,
    getTrademarksR
    :: Handler Html
getTermsR = defaultLayoutNew "terms-of-use" $ do
    snowdriftTitle "Terms of Use"
    alphaRewriteNotice
    renderDoc "Terms of Use"
getPrivacyR = defaultLayoutNew "privacy" $ do
    snowdriftTitle "Privacy Policy"
    alphaRewriteNotice
    renderDoc "Privacy Policy"
getTrademarksR = defaultLayoutNew "trademarks" $ do
    snowdriftTitle "Trademarks"
    alphaRewriteNotice
    renderDoc "Trademarks"

-- | Permanent redirects for legacy urls that may still be referenced
-- outside of the type-safe project
getLegacyTouR,
    getLegacyPrivR,
    getLegacyJsLicenseR
    :: Handler Html
getLegacyTouR       = redirectWith movedPermanently301 TermsR
getLegacyPrivR      = redirectWith movedPermanently301 PrivacyR
getLegacyJsLicenseR = redirectWith movedPermanently301 JsLicensesR
