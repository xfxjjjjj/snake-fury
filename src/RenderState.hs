
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}


{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X


-}
module RenderState where

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array, assocs )
import Data.Foldable ( foldl' )
import Data.ByteString.Builder

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.State.Class (MonadState, modify, gets)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Cont (MonadIO, liftIO)
import System.IO (stdout)

newtype RenderStep m a = RenderStep {runRenderStep :: ReaderT BoardInfo (StateT RenderState m) a}
  deriving (Functor, Applicative, Monad, MonadState RenderState, MonadReader BoardInfo)
-- A point is just a tuple of integers.
type Point = (Int, Int)

class HasRenderState s where
  getRenderState :: s -> RenderState
  setRenderState :: s -> RenderState -> s

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType     -- ^The board is an Array indexed by points with elements of type CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage = RenderBoard DeltaBoard | GameOver | Score deriving Show

-- | The RenderState contains the board and if the game is over or not.
data RenderState   = RenderState {
  board :: Board,
  gameOver :: Bool,
  score :: Int} deriving Show

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid b = let h = height b
                  w = width b
              in listArray ((1,1), (h,w)) (repeat Empty)

{-
This is a test for emptyGrid. It should return
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}
-- >>> emptyGrid (BoardInfo 2 2)
-- array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]


-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard i snake apple =
  let bd = emptyGrid i // [(snake, Snake), (apple, Apple)]
  in RenderState {board = bd, gameOver = False, score = 0}

{-
This is a test for buildInitialBoard. It should return
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}
-}
-- >>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- RenderState {board = array ((1,1),(2,2)) [((1,1),Snake),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}

updateRenderStates :: (HasRenderState s, MonadState s m, MonadReader BoardInfo m) => [RenderMessage] -> m ()
updateRenderStates = mapM_ updateRenderState

-- | Given tye current render state, and a message -> update the render state
updateRenderState :: (HasRenderState s, MonadState s m, MonadReader BoardInfo m) =>  RenderMessage -> m ()
updateRenderState GameOver = do
  st <- gets getRenderState
  modify $ flip setRenderState st {gameOver = True}
updateRenderState (RenderBoard delta) = do
  st@(RenderState bd _ _) <- gets getRenderState
  modify $ flip setRenderState st {board = bd // delta}
updateRenderState Score = do
  st <- gets getRenderState
  modify $ flip setRenderState st {score = succ (score st)}

{-
This is a test for updateRenderState

message1 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}

message2 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}
-}
-- >>> initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- >>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
-- >>> message2 = GameOver
-- >>> updateRenderState initial_board message1
-- >>> updateRenderState initial_board message2
-- RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}
-- RenderState {board = array ((1,1),(2,2)) [((1,1),Snake),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}


-- | Provisional Pretty printer
--   For each cell type choose a string to representing.
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some charachter followed by an space.
ppCell :: CellType -> Builder
ppCell Empty     = "- "
ppCell Snake     = "0 "
ppCell SnakeHead = "$ "
ppCell Apple     = "X "

renderStep :: (HasRenderState s, MonadState s m, MonadReader BoardInfo m) => [RenderMessage] -> m Builder
renderStep msgs = do
  updateRenderStates msgs
  (RenderState bd gg sc) <- gets getRenderState
  case gg of
    True -> do return $ "final score: " <> intDec sc
    _    -> do
      BoardInfo _ w <- ask
      let renderWith :: Builder -> (Point, CellType) -> Builder
          renderWith s ((_,w'), cell)
            | w' == w   = ppCell cell <> ("\n" <> s)
            | otherwise = ppCell cell <> s
      return $ foldl' renderWith "\n" (reverse (assocs bd)) <> ppScore sc

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
render :: (MonadReader BoardInfo m, MonadState s m, HasRenderState s, MonadIO m) => [RenderMessage] -> m ()
render msgs = do
  out <- renderStep msgs
  liftIO $ putStr "\ESC[2J" --This cleans the console screen
  liftIO $ hPutBuilder stdout out

ppScore :: Int -> Builder
ppScore n = let scoreLine = "score:" <> intDec n <> "\n"
                stars = "********\n"
            in  stars <> scoreLine <> stars

{-
This is a test for render. It should return:
"- - - - \n- 0 $ - \n- - - X \n"

Notice, that this depends on what you've chosen for ppCell
-}
-- >>> board = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
-- >>> board_info = BoardInfo 3 4
-- >>> render_state = RenderState board  False
-- >>> render board_info render_state
-- Couldn't match expected type `RenderState'
--             with actual type `Int -> RenderState'
-- Probable cause: `render_state' is applied to too few arguments
-- In the second argument of `render', namely `render_state'
-- In the expression: render board_info render_state
-- In an equation for `it_a1ph0':
--     it_a1ph0 = render board_info render_state
