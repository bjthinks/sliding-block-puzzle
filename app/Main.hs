module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform

-- This is always (y, x)
type Coord = (Int, Int)

data Environment = Environment
  { _vty :: Vty
  , _boardSize :: Coord
  }

$(makeLenses ''Environment)

type Board = Array Coord Int

data GameState = GameState
  { _terminalSize :: Coord
  , _board :: Board
  }

$(makeLenses ''GameState)

type Game = MaybeT (StateT GameState (ReaderT Environment IO))

solvedBoard :: Coord -> Board
solvedBoard bs = listArray ((1, 1), bs) $ [1..(fst bs * snd bs - 1)] ++ [0]

playGame :: Game String
playGame = do
  liftIO $ threadDelay 3000000
  b <- use board
  return $ show b

startGame :: Coord -> Vty -> IO (Maybe String)
startGame bs v = do
  let env = Environment { _vty = v, _boardSize = bs }
  db <- displayBounds $ outputIface v
  let startState = GameState
        { _terminalSize = db
        , _board = solvedBoard bs
        }
  runReaderT (evalStateT (runMaybeT playGame) startState) env

main :: IO ()
main = do
  msg <- bracket (mkVty defaultConfig) shutdown (startGame (3, 4))
  mapM_ putStrLn msg
