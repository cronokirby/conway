{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game (startGame) where

import Life (Pos)
import SDL
import Prelude

startGame :: IO ()
startGame = do
  initializeAll
  window <- createWindow "Conway" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let s = initialGameState renderer
  ts <- ticks
  runGame s (loop ts)

-- An action that the user can take
data Action = EscapeAction | PauseAction | NoAction | HoverAction Pos

eventToAction :: Event -> Action
eventToAction event = case eventPayload event of
  KeyboardEvent k | keyboardEventKeyMotion k == Pressed -> case keysymKeycode (keyboardEventKeysym k) of
    KeycodeEscape -> EscapeAction
    KeycodeSpace -> PauseAction
    _ -> NoAction
  MouseMotionEvent e ->
    let (P (V2 x y)) = mouseMotionEventPos e
     in HoverAction (fromIntegral (x `div` 20), fromIntegral (y `div` 20))
  _ -> NoAction

data GameState = GameState {paused :: Bool, shouldQuit :: Bool, rdr :: Renderer, timeBuf :: Word32, hover :: Pos}

initialGameState :: Renderer -> GameState
initialGameState rdr' = GameState True False rdr' 0 (0, 0)

newtype GameM a = GameM (ReaderT (IORef GameState) IO a) deriving (Functor, Applicative, Monad, MonadReader (IORef GameState), MonadIO)

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
  hvr' <- gets hover
  drawSquareOutline (V4 0 0 255 255) hvr'
  present rdr'
  unlessM (gets shouldQuit) (loop newTs)

rectFromPos :: Num a => Pos -> Rectangle a
rectFromPos (x, y) =
  let p = P (V2 (fromIntegral (x * 20)) (fromIntegral (y * 20)))
   in Rectangle p (V2 20 20)

drawSquareOutline :: V4 Word8 -> Pos -> GameM ()
drawSquareOutline color pos = do
  rdr' <- gets rdr
  rendererDrawColor rdr' $= color
  drawRect rdr' (Just (rectFromPos pos))

handleAction :: Action -> GameM ()
handleAction a = case a of
  EscapeAction -> modify (\s -> s {shouldQuit = True})
  PauseAction -> modify (\s -> s {paused = not (paused s)})
  HoverAction pos -> modify (\s -> s {hover = pos})
  NoAction -> return ()

handleTime :: GameM ()
handleTime = do
  tm <- gets timeBuf
  if tm > 500
    then modify (\s -> s {timeBuf = 0})
    else return ()
