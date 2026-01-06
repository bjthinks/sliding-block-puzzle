module Main where

import Control.Concurrent (threadDelay)
import qualified SDL.Init as I
import SDL.Mixer

main :: IO ()
main = do
  I.initialize [I.InitAudio]

  openAudio (Audio 48000 FormatS16_Sys Stereo) 1024
  slideSound <- load "slide.wav"

  play slideSound
  threadDelay 1000000

  halt AllChannels
  free slideSound
  closeAudio

  I.quit
