{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module App where
import GameState (GameState, Event, move, HasGameState (getGameState, setGameState))
import RenderState (RenderState (score, gameOver), BoardInfo, buildBoard,
 HasRenderState (getRenderState, setRenderState), HasBoardInfo (getBoardInfo),
 RenderMessage, updateRenderStates)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, gets, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue (..), readEvent, calculateSpeed, )
import Control.Concurrent (threadDelay, swapMVar)
import Control.Monad (unless, forever)

import System.IO (stdout)
import Data.ByteString.Builder

-- This is the new state, which glue together Game and Render states.
data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue

-- Our application is a readerT with and AppState and IO capabilities.
newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

class HasEventQueue env where
  getEventQueue :: env -> EventQueue

class Monad m => MonadQueue m where
  pullEvent :: m Event

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()

-- We need to make AppState and instance of HasGameState so we can use it with functions from `GameState.hs`
instance HasGameState AppState where
  getGameState :: AppState -> GameState
  getGameState (AppState gs _) = gs
  setGameState :: AppState -> GameState -> AppState
  setGameState (AppState _ rs) gs = AppState gs rs

-- We need to make AppState and instance of HasRenderState so we can use it with functions from `RenderState.hs`
instance HasRenderState AppState where
  getRenderState :: AppState -> RenderState
  getRenderState (AppState _ rs ) = rs
  setRenderState :: AppState -> RenderState -> AppState
  setRenderState (AppState gs _) = AppState gs

instance HasBoardInfo Env where
  getBoardInfo :: Env -> BoardInfo
  getBoardInfo (Env binf _) = binf

instance HasEventQueue Env where
  getEventQueue :: Env -> EventQueue
  getEventQueue (Env _ q) = q

instance (MonadIO m) => MonadQueue (App m) where -- you will need to fill the ???
  pullEvent :: Monad m => App m Event
  pullEvent = do
     eventQueue <- asks getEventQueue
     liftIO $ readEvent eventQueue

instance Monad m => MonadSnake (App m) where -- you will need to fill the ???
  updateGameState :: Monad m => Event -> App m [RenderMessage]
  updateGameState = move
  updateRenderState :: Monad m => [RenderMessage] -> App m ()
  updateRenderState = updateRenderStates

instance MonadIO m => MonadRender (App m) where -- you will need to fill the ???
  render :: Monad m => App m ()
  render = do
    binf <- asks getBoardInfo
    rstate <- gets getRenderState
    liftIO $ putStr "\ESC[2J" --This cleans the console screen
    liftIO $ hPutBuilder stdout (buildBoard binf rstate)

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  event_queue <- asks getEventQueue
  sc <- gets (score . getRenderState)
  let curSpeed = initialSpeed event_queue
  let newSpeed = calculateSpeed sc curSpeed
  case compare curSpeed newSpeed of
    EQ -> pure curSpeed
    _  -> liftIO $ swapMVar (currentSpeed event_queue) newSpeed >> pure newSpeed

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >> render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: (MonadQueue m, MonadSnake m, MonadRender m, MonadState state m, HasRenderState state, MonadReader env m, HasEventQueue env, MonadIO m) => m ()
gameloop = do
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- gets (gameOver . getRenderState)
  unless isGameOver gameloop

-- Run the application as usual
run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app
