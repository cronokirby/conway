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
  runGame s loop

-- An action that the user can take
data Action = EscapeAction | PauseAction | NoAction

eventToAction :: Event -> Action
eventToAction event = case eventPayload event of
  KeyboardEvent k | keyboardEventKeyMotion k == Pressed -> case keysymKeycode (keyboardEventKeysym k) of
    KeycodeEscape -> EscapeAction
    KeycodeSpace -> PauseAction
    _ -> NoAction
  _ -> NoAction

data GameState = GameState {paused :: Bool, shouldQuit :: Bool, rdr :: Renderer, count :: Word8}

initialGameState :: Renderer -> GameState
initialGameState rdr' = GameState True False rdr' 0

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

loop :: GameM ()
loop = do
  events <- pollEvents
  forM_ events (handleAction . eventToAction)
  unlessM (gets paused) (modify (\s -> s {count = (count s) + 1}))
  count' <- gets count
  rdr' <- gets rdr
  rendererDrawColor rdr' $= V4 (255 - count') count' 255 255
  clear rdr'
  present rdr'
  unlessM (gets shouldQuit) loop

handleAction :: Action -> GameM ()
handleAction a = case a of
  EscapeAction -> modify (\s -> s {shouldQuit = True})
  PauseAction -> modify (\s -> s {paused = not (paused s)})
  NoAction -> return ()
