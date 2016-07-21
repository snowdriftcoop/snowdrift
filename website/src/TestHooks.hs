{-# LANGUAGE CPP #-}

-- | TestHooks puts all dicey test-environment overrides in one place.
module TestHooks (middleware, sendVerifyEmail) where

import Import.NoFoundation

-- | Don't use CSRF in testing.
middleware :: (Yesod site, ToTypedContent res)
           => HandlerT site IO res -> HandlerT site IO res
middleware = addCsrf . defaultYesodMiddleware
  where
#if DEVELOPMENT
    addCsrf = id
#else
    addCsrf = defaultCsrfMiddleware
#endif

-- | Don't actually send email in testing OR development.
-- FIXME: Do send email in production, though :D
sendVerifyEmail :: MonadLogger m => Text -> t -> Text -> m ()
sendVerifyEmail e _k u = $logDebug ("Hit up " <> u <> " to authorize " <> e <> " .")
