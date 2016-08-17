{-# LANGUAGE CPP #-}

-- | TestHooks puts all dicey test-environment overrides in one place.
module TestHooks (middleware) where

import Import.NoFoundation

-- | Don't use CSRF in testing.
--
-- Note that "sslOnlyMiddleware" only prevents downgrades. "HSTS headers
-- over HTTP are ignored."
middleware :: (Yesod site, ToTypedContent res)
           => HandlerT site IO res -> HandlerT site IO res
middleware = sslOnlyMiddleware twoHours . addCsrf . defaultYesodMiddleware
  where
    twoHours = 120
#if DEVELOPMENT
    addCsrf = id
#else
    addCsrf = defaultCsrfMiddleware
#endif
