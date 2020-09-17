{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game (startGame) where

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
data Action = EscapeAction | PauseAction | NoAction

eventToAction :: Event -> Action
eventToAction event = case eventPayload event of
  KeyboardEvent k | keyboardEventKeyMotion k == Pressed -> case keysymKeycode (keyboardEventKeysym k) of
    KeycodeEscape -> EscapeAction
    KeycodeSpace -> PauseAction
    _ -> NoAction
  _ -> NoAction

data GameState = GameState {paused :: Bool, shouldQuit :: Bool, rdr :: Renderer, timeBuf :: Word32, flp :: Bool}

initialGameState :: Renderer -> GameState
initialGameState rdr' = GameState True False rdr' 0 False

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
  color <- ifM (gets flp) (return (V4 0 0 255 255)) (return (V4 0 255 0 255))
  rendererDrawColor rdr' $= color
  clear rdr'
  present rdr'
  unlessM (gets shouldQuit) (loop newTs)

handleAction :: Action -> GameM ()
handleAction a = case a of
  EscapeAction -> modify (\s -> s {shouldQuit = True})
  PauseAction -> modify (\s -> s {paused = not (paused s)})
  NoAction -> return ()

handleTime :: GameM ()
handleTime = do
  tm <- gets timeBuf
  if tm > 500
    then modify (\s -> s {timeBuf = 0, flp = not (flp s)})
    else return ()
