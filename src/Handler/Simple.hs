module Handler.Simple where

import Import

import Network.HTTP.Types.Status (movedPermanently301)

import Dev
import Handler.TH

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
    :: Handler Html
getIntroR              = $(widget "how-it-works/intro" "Intro")
getFloR                = $(widget "how-it-works/flo" "FLO")
getNetworkEffectR      = $(widget "how-it-works/network-effect"
                                         "Network Effect")
getSustainableFundingR = $(widget "how-it-works/sustainable-funding"
                                         "Sustainable Funding")
getCoOpR               = $(widget "how-it-works/co-op" "Co-op")
getPSignupR            = $(widget "project-signup" "Project Signup")
getAboutHomeR          = $(widget "about/old-homepage" "About")
getContactR            = $(widget "contact" "Contact")
getTeamR               = $(widget "team" "Team")
getPressR              = $(widget "press" "Press")
getSponsorsR           = $(widget "sponsors" "Sponsors")
getDonateR             = $(widget "donate" "Donate")
getMerchandiseR        = $(widget "merchandise" "Merchandise")

-- * TODO: Convert these.
getTermsR,
    getPrivacyR,
    getTrademarksR
    :: Handler Html
getTermsR = undefined
getPrivacyR = undefined
getTrademarksR = undefined

-- | Permanent redirects for legacy urls that may still be referenced
-- outside of the type-safe project
getLegacyTouR,
    getLegacyPrivR,
    getLegacyJsLicenseR
    :: Handler Html
getLegacyTouR       = redirectWith movedPermanently301 TermsR
getLegacyPrivR      = redirectWith movedPermanently301 PrivacyR
getLegacyJsLicenseR = redirectWith movedPermanently301 JsLicensesR
