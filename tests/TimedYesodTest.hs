module TimedYesodTest
    (module Yesod.Test
    ,yit
    ) where


import Yesod.Test hiding (yit)
import qualified Yesod.Test as Y

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time
import Text.Printf

-- | Prepend timing information to Yesod.Test's 'yit'
yit :: String -> YesodExample site () -> YesodSpec site
yit desc act = Y.yit desc (timeIt act)

-- Inspired by the timeit package:
-- http://hackage.haskell.org/package/timeit-1.0.0.0
timeIt :: MonadIO m => m a -> m a
timeIt ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf " (%2.2fs)" t
    return a

timeItT :: MonadIO m => m a -> m (Double, a)
timeItT ioa = do
    t1 <- liftIO getCurrentTime
    a  <- ioa
    t2 <- liftIO getCurrentTime
    let t = fromRational . toRational $ diffUTCTime t2 t1
    return (t, a)
