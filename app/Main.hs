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

bot :: a
bot = bot

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
  , _tileSize :: Coord
  , _tileImages :: Array Int Image
  , _basePicture :: Picture
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
  sequence_ $ replicate (4 * (fst bs)^two * (snd bs)^two)
    (randomMove >> displayPuzzle)

style :: Attr
style = defAttr `withForeColor` brightWhite `withBackColor` black

tileImage :: Int -> Game Image
tileImage t = do
  (rows, cols) <- use tileSize
  let horizontal = replicate (cols - 2) '\x2501'
      spaces = replicate (cols - 2) ' '
      top    = "\x250f" ++ horizontal ++ "\x2513"
      bottom = "\x2517" ++ horizontal ++ "\x251b"
      middle = "\x2503" ++ spaces     ++ "\x2503"
      tStr = show t
      len = length tStr
      number = "\x2503" ++ replicate ((cols - 1 - len) `div` 2) ' ' ++ tStr ++
        replicate ((cols - 2 - len) `div` 2) ' ' ++ "\x2503"
      imageRows = [top] ++ replicate ((rows - 2) `div` 2) middle ++ [number] ++
        replicate ((rows - 3) `div` 2) middle ++ [bottom]
  return $ foldr1 (<->) $ map (string style) imageRows

makeTileImages :: Game ()
makeTileImages = do
  (y, x) <- view boardSize
  images <- sequence $ map tileImage [0..x*y - 1]
  tileImages .= listArray (0, x*y - 1) images

setBasePicture :: Game ()
setBasePicture = do
  (_, numRows) <- use terminalSize
  let help = "Wait one moment and the program will exit"
  basePicture .= (picForImage $ translate 0 (numRows - 1) $ string style help)
    { picBackground = Background ' ' style }

displayPuzzle :: Game ()
displayPuzzle = do
  bp <- use basePicture
  ts <- use tileImages
  let wholePicture = addToTop bp (ts ! 11)
  v <- view vty
  liftIO $ update v wholePicture

playGame :: Game String
playGame = do
  setBasePicture
  makeTileImages
  displayPuzzle
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
        , _tileSize = (5, 7) -- TODO fix this
        , _tileImages = bot
        , _basePicture = bot
        }
  runReaderT (evalStateT (evalRandT (runMaybeT playGame) gen) startState) env

main :: IO ()
main = do
  msg <- bracket (mkVty defaultConfig) shutdown (startGame (3, 4))
  mapM_ putStrLn msg
