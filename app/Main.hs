{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Array
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Lens.Micro.Platform
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
#ifdef SOUND
import qualified Data.ByteString as BS
import Data.FileEmbed
import qualified SDL.Init as SDLI
import qualified SDL.Mixer as SDLM
#endif

bot :: a
bot = bot

-- This is always (y, x)
type Coord = (Int, Int)
type Board = Array Coord Int

data Environment = Environment
  { _vty :: Vty
#ifdef SOUND
  , _slideSound :: SDLM.Chunk
#endif
  , _boardSize :: Coord
  , _goal :: Board
  }

$(makeLenses ''Environment)

data GameState = GameState
  { _terminalSize :: Coord
  , _board :: Board
  , _blank :: Coord
  , _tileSize :: Coord
  , _tileImages :: Array Int Image
  , _basePicture :: Picture
  , _movingTile :: Maybe (Coord, Coord)
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

setTileSize :: Game ()
setTileSize = do
  (by, bx) <- view boardSize
  (dy, dx) <- use terminalSize
  let ty = max 3 $ (dy - by) `div` by -- Leave one row at bottom for help message
      minWidth = 2 + length (show $ by * bx - 1)
      tx = max minWidth $ (dx - bx + 1) `div` bx
  tileSize .= (ty, tx)

style :: Attr
style = defAttr `withForeColor` brightWhite `withBackColor` black

inverseStyle :: Attr
inverseStyle = defAttr `withForeColor` black `withBackColor` brightWhite

tileImage :: Int -> Game Image
tileImage 0 = return emptyImage
tileImage t = do
  (rows, cols) <- use tileSize
  let spaces = replicate (cols - 2) ' '
      top    = " " ++ replicate (cols - 2) ' ' ++ " "
      bottom = " " ++ replicate (cols - 2) ' ' ++ " "
      middle = " " ++ spaces ++ " "
      tStr = show t
      len = length tStr
      number = " " ++ replicate ((cols - 1 - len) `div` 2) ' ' ++ tStr ++
        replicate ((cols - 2 - len) `div` 2) ' ' ++ " "
      imageRows = [top] ++ replicate ((rows - 2) `div` 2) middle ++ [number] ++
        replicate ((rows - 3) `div` 2) middle ++ [bottom]
  return $ vertCat $ map (string inverseStyle) imageRows

makeTileImages :: Game ()
makeTileImages = do
  (y, x) <- view boardSize
  images <- sequence $ map tileImage [0..x*y - 1]
  tileImages .= listArray (0, x*y - 1) images

makeMovingTileImage :: Game ()
makeMovingTileImage = do
  maybeMoving <- use movingTile
  -- (dy, dx) are in eighths of a pixel
  let (movingCoord, (dy, dx)) = maybe ((0,0), (0,0)) id maybeMoving
      (sy, sx) = (dy `mod` 8, dx `mod` 8)
  b <- use board
  let tile = b ! movingCoord
  (rows, cols) <- use tileSize
  let tileStr = show tile
      len = length tileStr
      number = replicate ((cols - 1 - len) `div` 2 + sx `div` 4) ' ' ++
        tileStr ++
        replicate ((cols - len) `div` 2 - sx `div` 4) ' '
  imageRows <-
    if sy /= 0
    then do
      return $
        [string style $ replicate cols (topChar sy)] ++
        map (string inverseStyle)
        (replicate ((rows - 2) `div` 2 + sy `div` 4) (replicate cols ' ') ++
         [' ' : number] ++
         replicate ((rows - 1) `div` 2 - sy `div` 4) (replicate cols ' ') ++
         [replicate cols (topChar sy)])
    else if sx /= 0
    then do
      return $
        replicate (rows `div` 2)
        (string inverseStyle
         ([leftChar sx] ++ replicate (cols - 1) ' ') <|>
         string style [leftChar sx]) ++
        [string inverseStyle ([leftChar sx] ++ number) <|>
         string style [leftChar sx]] ++
        (replicate ((rows - 1) `div` 2)
        (string inverseStyle
         ([leftChar sx] ++ replicate (cols - 1) ' ') <|>
         string style [leftChar sx]))
    else do
      return $
        map (string inverseStyle) $
        replicate (rows `div` 2) (replicate cols ' ') ++
        [' ' : number] ++
        replicate ((rows - 1) `div` 2) (replicate cols ' ')
  let image = vertCat imageRows
  oldTileImages <- use tileImages
  tileImages .= oldTileImages // [(tile, image)]
  where
    topChar 1 = '\x2587'
    topChar 2 = '\x2586'
    topChar 3 = '\x2585'
    topChar 4 = '\x2584'
    topChar 5 = '\x2583'
    topChar 6 = '\x2582'
    topChar 7 = '\x2581'
    topChar _ = undefined
    leftChar 1 = '\x258f'
    leftChar 2 = '\x258e'
    leftChar 3 = '\x258d'
    leftChar 4 = '\x258c'
    leftChar 5 = '\x258b'
    leftChar 6 = '\x258a'
    leftChar 7 = '\x2589'
    leftChar _ = undefined

setBasePicture :: Game ()
setBasePicture = do
  (numRows, _) <- use terminalSize
  let help = "Arrow keys to move, ESC or q to exit."
  basePicture .= (picForImage $ translate 0 (numRows - 1) $ string style help)
    { picBackground = Background ' ' style }

displayPuzzle :: Game ()
displayPuzzle = do
  bp <- use basePicture
  b <- use board
  ts <- use tileImages
  (tileHeight, tileWidth) <- use tileSize
  maybeMoving <- use movingTile
  -- (dy, dx) are in eighths of a pixel
  let (movingCoord, (dy, dx)) = maybe ((0,0), (0,0)) id maybeMoving
      (py, px) = (dy `div` 8, dx `div` 8)
  let translatedTiles =
        [ (if (y, x) == movingCoord then (translate px py $) else id) $
          translate ((tileWidth+1)*(x-1)) ((tileHeight+1)*(y-1)) (ts ! t)
        | ((y, x), t) <- assocs b]
  let wholePicture = foldr (flip addToTop) bp translatedTiles
  v <- view vty
  liftIO $ update v wholePicture

handleResize :: Coord -> Game ()
handleResize newSize = do
  terminalSize .= newSize
  setBasePicture
  setTileSize
  makeTileImages

animate :: Int -> Coord -> [Coord] -> Game ()
animate _ _ [] = undefined
animate usec (y, x) ((dy, dx):ds) = do
  movingTile .= Just ((y, x), (dy, dx))
  makeMovingTileImage
  displayPuzzle
  if ds /= [] then do
    liftIO $ threadDelay usec
    animate usec (y, x) ds
    else return ()

moveTo :: Coord -> Coord -> Game ()
moveTo p1 p2 = do
  b <- use board
  let t1 = b ! p1
      t2 = b ! p2
  board .= b // [(p1, t2), (p2, t1)]

moveUp :: Game ()
moveUp = do
  (y, x) <- use blank
  (ymax, _) <- view boardSize
  if y < ymax then
    do let y' = y+1
       playSlide
       (tileHeight, _) <- use tileSize
       let steps = 8 * tileHeight + 8
           offsets = [(o, 0) | o <- [(-1),(-2)..(-steps)]]
           delay = 250000 `div` (steps - 1)
       animate delay (y', x) offsets
       moveTo (y', x) (y, x)
       blank .= (y', x)
    else return ()

moveDown :: Game ()
moveDown = do
  (y, x) <- use blank
  if y > 1 then
    do let y' = y-1
       playSlide
       (tileHeight, _) <- use tileSize
       let steps = 8 * tileHeight + 8
           offsets = [(o, 0) | o <- [1,2..steps]]
           delay = 250000 `div` (steps - 1)
       animate delay (y', x) offsets
       moveTo (y', x) (y, x)
       blank .= (y', x)
    else return ()

moveLeft :: Game ()
moveLeft = do
  (y, x) <- use blank
  (_, xmax) <- view boardSize
  if x < xmax then
    do let x' = x+1
       playSlide
       (_, tileWidth) <- use tileSize
       let steps = 8 * tileWidth + 8
           offsets = [(0, o) | o <- [(-1),(-2)..(-steps)]]
           delay = 250000 `div` (steps - 1)
       animate delay (y, x') offsets
       moveTo (y, x') (y, x)
       blank .= (y, x')
    else return ()

moveRight :: Game ()
moveRight = do
  (y, x) <- use blank
  if x > 1 then
    do let x' = x-1
       playSlide
       (_, tileWidth) <- use tileSize
       let steps = 8 * tileWidth + 8
           offsets = [(0, o) | o <- [1,2..steps]]
           delay = 250000 `div` (steps - 1)
       animate delay (y, x') offsets
       moveTo (y, x') (y, x)
       blank .= (y, x')
    else return ()

#ifdef SOUND
slideWavData :: BS.ByteString
slideWavData = $(embedFile "short-slide.wav")
#endif

playSlide :: Game ()
#ifdef SOUND
playSlide = do
  playing <- SDLM.playing (fromInteger 1)
  unless playing $ do
    ss <- view slideSound
    _ <- SDLM.playOn (fromInteger 1) SDLM.Once ss
    return ()
#else
playSlide = return ()
#endif

eventLoop :: Game String
eventLoop = do
  displayPuzzle
  v <- view vty
  e <- liftIO $ nextEvent v
  case e of
    EvKey KEsc _ -> mzero
    EvKey (KChar 'q') _ -> mzero
    EvKey (KChar 'Q') _ -> mzero
    EvKey KUp    _ -> moveUp
    EvKey KDown  _ -> moveDown
    EvKey KLeft  _ -> moveLeft
    EvKey KRight _ -> moveRight
    EvResize c r -> handleResize (r, c)
    _ -> return ()
  b <- use board
  g <- view goal
  if b == g
    then return $
         "Congratulations! For a greater challenge, " ++
         "try changing the board size.\n" ++
         "To do that: cabal run sliding-block-puzzle -- width height\n"
    else eventLoop

playGame :: Game String
playGame = do
  setBasePicture
  setTileSize
  makeTileImages
  displayPuzzle
  scramblePuzzle
  eventLoop

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
  (MkSystemTime s ns) <- getSystemTime
  return $ toInteger s * 10^(9 :: Int) + toInteger ns

#ifdef SOUND
type StartParams = (Vty, SDLM.Chunk)
#else
type StartParams = Vty
#endif

startGame :: Coord -> StartParams -> IO (Maybe String)
#ifdef SOUND
startGame bs (v, ss) = do
#else
startGame bs v = do
#endif
  let env = Environment
        { _vty = v
#ifdef SOUND
        , _slideSound = ss
#endif
        , _boardSize = bs
        , _goal = solvedBoard bs
        }
  t <- getNanosSinceEpoch
  let gen = mkStdGen $ fromInteger t
  (cols, rows) <- displayBounds $ outputIface v
  let startState = GameState
        { _terminalSize = (rows, cols)
        , _board = solvedBoard bs
        , _blank = bs
        , _tileSize = bot
        , _tileImages = bot
        , _basePicture = bot
        , _movingTile = Nothing
        }
  runReaderT (evalStateT (evalRandT (runMaybeT playGame) gen) startState) env

main :: IO ()
main = do
  args <- getArgs
  bs <- case args of
    [x, y] -> return $ (read y, read x)
    [] -> return (4, 4)
    _ -> hPutStrLn stderr "Please specify a width and height." >> exitFailure
  when (fst bs > 16 || snd bs > 16) $
    hPutStrLn stderr "Error: puzzle too large. Maximum is 16 by 16." >> exitFailure
  msg <- bracket
    (do
#ifdef SOUND
        SDLI.initialize [SDLI.InitAudio]
        SDLM.openAudio (SDLM.Audio 48000 SDLM.FormatS16_Sys SDLM.Stereo) 1024
        ss <- SDLM.decode slideWavData
#endif
        v <- mkVty defaultConfig
#ifdef SOUND
        return (v, ss))
    (\(v, ss) ->
#else
        return v)
    (\v ->
#endif
       do shutdown v
#ifdef SOUND
          SDLM.halt SDLM.AllChannels
          SDLM.free ss
          SDLM.closeAudio
#endif
    )
    (startGame bs)
  mapM_ putStr msg
