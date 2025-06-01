{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, StdGen )

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State.Strict (State, get, put, runState)

type GameStep a = ReaderT BoardInfo (State GameState) a

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East
-- True
-- True
-- True
-- True


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation.
--   Also, in the import list you have all relevant functions.
makeRandomPoint ::  GameStep Point
makeRandomPoint = do
    BoardInfo h w <- ask
    st <- lift get
    let (p, g') = uniformR ((1,1),(h,w)) (randomGen st)
    lift (put st {randomGen = g'})
    return p

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake xy (SnakeSeq h ts) = h == xy || elem xy ts

{-
This is a test for inSnake. It should return
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (SnakeSeq (x,y) _) _ dir _) = case dir of
  North -> if x == 1 then (h,y) else (x-1, y)
  South -> if x == h then (1,y) else (x+1, y)
  East  -> if y == w then (x,1) else (x, y+1)
  West  -> if y == 1 then (x,w) else (x, y-1)

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: GameStep Point
newApple = findValidApple
  where
    findValidApple =
      do
        st <- lift get
        p <- makeRandomPoint
        let (SnakeSeq he ts) = snakeSeq st
        if p == applePosition st || p == he || elem p ts
          then findValidApple
          else return p



{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
--

extendSnake :: Point -> GameStep RenderState.DeltaBoard
extendSnake p = do
    st <- lift get
    app <- newApple
    let (SnakeSeq he ts) = snakeSeq st
        delta = [(p, Board.SnakeHead), (he, Board.Snake), (app, Board.Apple)]
        snakeSe = SnakeSeq p (he:<|ts)
    lift (put st {snakeSeq = snakeSe, applePosition = app})
    return delta

displaceSnake :: Point -> GameStep RenderState.DeltaBoard
displaceSnake p = do
  st <- lift get
  let (SnakeSeq he ts) = snakeSeq st
  case ts of
        S.Empty -> do
          let delta = [(p, Board.SnakeHead), (he, Board.Empty)]
              snakeSe = SnakeSeq {snakeHead = p, snakeBody = S.Empty}
          lift (put st {snakeSeq = snakeSe})
          return delta
        x :<| S.Empty -> do
          let delta = [(p, Board.SnakeHead), (he, Board.Snake), (x, Board.Empty)]
              snakeSe = SnakeSeq {snakeHead = p, snakeBody = S.singleton he}
          lift (put st {snakeSeq = snakeSe})
          return delta
        x :<| (xs :|> t) -> do
          let delta = [(p, Board.SnakeHead), (he, Board.Snake), (t, Board.Empty)]
              snakeSe = SnakeSeq {snakeHead = p, snakeBody = he :<| x :<| xs}
          lift (put st {snakeSeq = snakeSe})
          return delta

step :: GameStep [Board.RenderMessage]
step = do
  info <- ask
  st <- lift get
  let p = nextHead info st
      (SnakeSeq _ ts) = snakeSeq st
  if p `elem` ts then return [Board.GameOver]
  else case compare p (applePosition st) of
        EQ -> do
          rb <- extendSnake p
          return [Board.Score, Board.RenderBoard rb]
        _  -> do
          rb <- displaceSnake p
          return [Board.RenderBoard rb]

move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move = runState . runReaderT step

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]
