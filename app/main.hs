import Prelude (IO)

import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMainLog)

import Application (makeApplication)
import Settings (parseExtra)

main :: IO ()
main = defaultMainLog (fromArgs parseExtra) makeApplication
