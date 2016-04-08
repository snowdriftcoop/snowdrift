{-# LANGUAGE CPP #-}

-- | TestHooks puts all dicey test-environment overrides in one place.
module TestHooks (middleware, authPlugins) where

import Import.NoFoundation hiding (authPlugins)

#if TESTING
import Yesod.Auth.Dummy (authDummy)
#endif
import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))

-- | Don't use CSRF in testing.
middleware :: (Yesod site, ToTypedContent res)
           => HandlerT site IO res -> HandlerT site IO res
middleware = addCsrf . defaultYesodMiddleware
  where
#if TESTING
    addCsrf = id
#else
    addCsrf = defaultCsrfMiddleware
#endif

-- | Enable dummy auth (Yesod.Auth.Dummy) in testing.
authPlugins :: YesodAuth master => [AuthPlugin master]
authPlugins = addDummy [authOpenId Claimed []]
  where
#if TESTING
    addDummy = (authDummy :)
#else
    addDummy = id
#endif
