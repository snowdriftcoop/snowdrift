{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module TestHandler where

import Import

import Control.Concurrent.STM
import Network.Wai.Logger
import System.Log.FastLogger
import Yesod.Core.Types
import Yesod.Default.Config

testHandler :: Handler a -> IO (Either ErrorResponse a)
testHandler handler = do
    events <- atomically newTChan :: IO (TChan SnowdriftEvent)
    logger_set <- System.Log.FastLogger.newStderrLoggerSet 4096

    let extra = Extra "copyright" "sourcerepo" (Just "ghrepo") "siteproject" Nothing
        config = AppConfig Development 3000 "http://localhost:3000" (error "HostPreferences") extra
        app = App (return ()) config (error "StaticSettings") (error "PersistConfigPool") (error "Manager") (error "PersistConf") (error "Logger") events (const [])

    (date_getter, date_updater) <- Network.Wai.Logger.clockDateCacher

    date_updater

    runFakeHandler mempty (const $ Logger logger_set date_getter) app handler

