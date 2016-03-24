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
getIntroR              = $(widget "page/how-it-works/intro" "Intro")
getFloR                = $(widget "page/how-it-works/flo" "FLO")
getNetworkEffectR      = $(widget "page/how-it-works/network-effect"
                                         "Network Effect")
getSustainableFundingR = $(widget "page/how-it-works/sustainable-funding"
                                         "Sustainable Funding")
getCoOpR               = $(widget "page/how-it-works/co-op" "Co-op")
getPSignupR            = $(widget "page/project-signup" "Project Signup")
getAboutHomeR          = $(widget "page/about/old-homepage" "About")
getContactR            = $(widget "page/contact" "Contact")
getTeamR               = $(widget "page/team" "Team")
getPressR              = $(widget "page/press" "Press")
getSponsorsR           = $(widget "page/sponsors" "Sponsors")
getDonateR             = $(widget "page/donate" "Donate")
getMerchandiseR        = $(widget "page/merchandise" "Merchandise")

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
