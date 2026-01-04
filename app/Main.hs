module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Graphics.Vty
import Graphics.Vty.CrossPlatform

main :: IO ()
main = bracket (mkVty defaultConfig) shutdown $ const $ threadDelay 3000000
