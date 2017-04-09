module Main (main) where

import Crowdmatch
import RunPersist

main :: IO ()
main = crowdmatch runPersist
