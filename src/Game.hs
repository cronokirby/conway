{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game (startGame) where

import Life
import SDL
import Prelude

-- The number of pixels per cell
cellFactor :: Num a => a
cellFactor = 20

-- This is the number of milliseconds between state transitions, to begin with.
initialLimitTime :: Int
initialLimitTime = 250

-- Start running the game
--
-- This will do the necessary things for initializing SDL, creating a window, etc. etc.
startGame :: IO ()
startGame = do
  initializeAll
  window <- createWindow "Conway" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let s = initialGameState renderer
  ts <- ticks
  runGame s (loop ts)

-- An action that the user can take
data Action
  -- The user wants to exit the game
  = EscapeAction
  -- The user wants to pause the game
  | PauseAction
  -- The user wants to increase the speed of the game
  | AccelAction
  -- The user wants to decrease the speed of the game
  | DecelAction
  -- The user wants to advance the game forward by a single "turn"
  | StepAction
  -- The user is hovering over a specific cell
  | HoverAction Pos
  -- The user has clicked on a cell
  | ClickAction Pos
  -- This is the action we return when we have an event we don't recognize
  | NoAction

eventToAction :: Event -> Action
eventToAction event = case eventPayload event of
  KeyboardEvent k | keyboardEventKeyMotion k == Pressed -> case keysymKeycode (keyboardEventKeysym k) of
    KeycodeEscape -> EscapeAction
    KeycodeSpace -> PauseAction
    KeycodeS -> StepAction
    KeycodeA -> AccelAction
    KeycodeD -> DecelAction
    _ -> NoAction
  MouseMotionEvent e ->
    let (P (V2 x y)) = mouseMotionEventPos e
     in HoverAction (fromIntegral (x `div` 20), fromIntegral (y `div` 20))
  MouseButtonEvent e
    | mouseButtonEventMotion e == Pressed ->
      let (P (V2 x y)) = mouseButtonEventPos e
       in ClickAction (fromIntegral (x `div` 20), fromIntegral (y `div` 20))
  _ -> NoAction

data GameState = GameState
  { paused :: Bool,
    shouldQuit :: Bool,
    rdr :: Renderer,
    timeBuf :: Word32,
    hover :: Pos,
    life :: Grid Life,
    limitTime :: Int
  }

-- The starting state of the game
initialGameState :: Renderer -> GameState
initialGameState rdr' = GameState True False rdr' 0 (0, 0) (emptyLife (800 `div` cellFactor) (600 `div` cellFactor)) initialLimitTime

-- The context where game logic happens
newtype GameM a = GameM (ReaderT (IORef GameState) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (IORef GameState), MonadIO)

-- Run the game, starting from a certain state
runGame :: GameState -> GameM a -> IO a
runGame s (GameM m) = do
  ref <- newIORef s
  runReaderT m ref

instance MonadState GameState GameM where
  get = do
    ref <- ask
    readIORef ref
  put s = do
    ref <- ask
    writeIORef ref s

-- This is the main game loop, taking the millisecond ticks as an arguments
loop :: Word32 -> GameM ()
loop ts = do
  events <- pollEvents
  forM_ events (handleAction . eventToAction)
  rdr' <- gets rdr
  newTs <- SDL.ticks
  unlessM (gets paused) (modify (\s -> s {timeBuf = (timeBuf s) + (newTs - ts)}))
  handleTime
  rendererDrawColor rdr' $= V4 0 0 0 255
  clear rdr'
  life' <- gets life
  drawLife (V4 255 255 255 255) life'
  hvr' <- gets hover
  drawSquareOutline (V4 0 0 255 255) hvr'
  present rdr'
  unlessM (gets shouldQuit) (loop newTs)

-- Create a rectangle from a position
rectFromPos :: Num a => Pos -> Rectangle a
rectFromPos (x, y) =
  let p = P (V2 (fromIntegral (x * cellFactor)) (fromIntegral (y * cellFactor)))
   in Rectangle p (V2 cellFactor cellFactor)

-- Draw an outline, given a color, and a position
drawSquareOutline :: V4 Word8 -> Pos -> GameM ()
drawSquareOutline color pos = do
  rdr' <- gets rdr
  rendererDrawColor rdr' $= color
  drawRect rdr' (Just (rectFromPos pos))

-- Draw all of the live cells in a grid for the game of life
drawLife :: V4 Word8 -> Grid Life -> GameM ()
drawLife color grid = do
  let poss = positions grid |> filter ((== Alive) . snd) |> map fst
  rdr' <- gets rdr
  rendererDrawColor rdr' $= color
  forM_ poss (\p -> fillRect rdr' (Just (rectFromPos p)))

-- Handle one of the fundamanetal game actions
handleAction :: Action -> GameM ()
handleAction a = case a of
  EscapeAction -> modify (\s -> s {shouldQuit = True})
  PauseAction -> modify (\s -> s {paused = not (paused s)})
  HoverAction pos -> modify (\s -> s {hover = pos})
  ClickAction pos -> modify (\s -> s {life = life s <> singleLife pos})
  StepAction -> step
  DecelAction -> modify (\s -> s {limitTime = (limitTime s) * 3 `div` 2})
  AccelAction -> modify (\s -> s {limitTime = max 10 ((limitTime s) * 2 `div` 3)})
  NoAction -> return ()

-- Handle the time progression of the game
--
-- This will advance the game of life using conway's rules whenever the limiting time is set
handleTime :: GameM ()
handleTime = do
  tm <- gets timeBuf
  limitTime' <- gets limitTime
  if tm > (fromIntegral limitTime')
    then do
      modify (\s -> s {timeBuf = 0})
      step
    else return ()

-- Advance the game of life by a single step
step :: GameM ()
step =
  modify (\s -> s {life = conway (life s)})
