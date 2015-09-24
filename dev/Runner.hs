module Runner (main, shutdown) where

import Prelude              (IO, return)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMainLog)
import Settings             (parseExtra)
import Application          (makeApplication)
import qualified Dev.Runner as Dev

main :: IO ()
main = Dev.update (defaultMainLog (fromArgs parseExtra) makeApplication) (return ())

shutdown :: IO ()
shutdown = Dev.shutdown
