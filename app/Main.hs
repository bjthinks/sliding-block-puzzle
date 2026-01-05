module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Vty
import Graphics.Vty.CrossPlatform

data Coord = Coord
  { _y :: Int
  , _x :: Int
  }

$(makeLenses ''Coord)

data Environment = Environment
  { _vty :: Vty
  , _boardSize :: Coord
  }

$(makeLenses ''Environment)

data GameState = GameState
  { _terminalSize :: Coord
  }

$(makeLenses ''GameState)

type Game = MaybeT (StateT GameState (ReaderT Environment IO))

playGame :: Game String
playGame = do
  liftIO $ threadDelay 3000000
  mzero

startGame :: Coord -> Vty -> IO (Maybe String)
startGame size v = do
  let env = Environment { _vty = v, _boardSize = size }
  let output = outputIface v
  (c, r) <- displayBounds output
  let startState = GameState { _terminalSize = Coord c r }
  runReaderT (evalStateT (runMaybeT playGame) startState) env

main :: IO ()
main = do
  msg <- bracket (mkVty defaultConfig) shutdown (startGame $ Coord 4 4)
  mapM_ putStrLn msg
