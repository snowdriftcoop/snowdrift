-- | This module defines methods needed for the YesodAuthEmail instance for
-- App.
module EmailAuth
        ( addUnverified
        , getVerifyKey
        , setVerifyKey
        , verifyAccount
        , getPassword
        , setPassword
        , getEmailCreds
        , getEmail
        ) where

import Import.NoFoundation

import qualified TestHooks

addUnverified e k = undefined
getVerifyKey eid = undefined
setVerifyKey eid k = undefined
verifyAccount eid = undefined
getPassword eid = undefined
setPassword eid saltedpass = undefined
getEmailCreds ident = undefined
getEmail eid = undefined
