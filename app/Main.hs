module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Array
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
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
  , _blank :: Coord
  }

$(makeLenses ''GameState)

type Game = MaybeT (RandT StdGen (StateT GameState (ReaderT Environment IO)))

solvedBoard :: Coord -> Board
solvedBoard bs = listArray ((1, 1), bs) $ [1..(fst bs * snd bs - 1)] ++ [0]

isValidPosition :: Coord -> Game Bool
isValidPosition (yy, xx) = do
  (ymax, xmax) <- view boardSize
  return $ yy >= 1 && yy <= ymax && xx >= 1 && xx <= xmax

randomMove :: Game ()
randomMove = do
  oldBoard <- use board
  oldBlank <- use blank
  let (oldBlankY, oldBlankX) = oldBlank
  possibilities <- filterM isValidPosition
    [(oldBlankY-1, oldBlankX), (oldBlankY, oldBlankX-1),
     (oldBlankY+1, oldBlankX), (oldBlankY, oldBlankX+1)]
  which <- lift $ getRandomR (0, length possibilities - 1)
  let newBlank = possibilities !! which
      tile = oldBoard ! newBlank
      newBoard = oldBoard // [(oldBlank, tile), (newBlank, 0)]
  board .= newBoard
  blank .= newBlank

scramblePuzzle :: Game ()
scramblePuzzle = do
  bs <- view boardSize
  -- This is good for up to a 16x16 puzzle
  let two = 2 :: Int
  sequence_ $ replicate (4 * (fst bs)^two * (snd bs)^two) randomMove

playGame :: Game String
playGame = do
  scramblePuzzle
  liftIO $ threadDelay 1000000
  b <- use board
  return $ show b

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
  (MkSystemTime s ns) <- getSystemTime
  return $ toInteger s * 10^(9 :: Int) + toInteger ns

startGame :: Coord -> Vty -> IO (Maybe String)
startGame bs v = do
  let env = Environment { _vty = v, _boardSize = bs }
  t <- getNanosSinceEpoch
  let gen = mkStdGen $ fromInteger t
  db <- displayBounds $ outputIface v
  let startState = GameState
        { _terminalSize = db
        , _board = solvedBoard bs
        , _blank = bs
        }
  runReaderT (evalStateT (evalRandT (runMaybeT playGame) gen) startState) env

main :: IO ()
main = do
  msg <- bracket (mkVty defaultConfig) shutdown (startGame (3, 4))
  mapM_ putStrLn msg
