-- | This module contains the data types
--   which represent the state of the game

module Model where

import Prelude hiding (lookup, zip3)
import Data.List hiding (lookup)
import Data.Sequence hiding (zip3, replicate, Empty)


{-
  Initialization
-}
initialGameTiles :: [Tile]
initialGameTiles = [
  w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w,
  w, d, d, d, d, d, d, d, d, d, d, d, d, w, w, d, d, d, d, d, d, d, d, d, d, d, d, w,
  w, d, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, d, w,
  w, d, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, d, w,
  w, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, w,
  w, d, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, d, w,
  w, d, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, d, w,
  w, d, d, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, d, d, w,
  w, w, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, w, w,
  e, e, e, e, e, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, e, e, e, e, e,
  e, e, e, e, e, w, d, w, w, d, d, d, d, d, d, d, d, d, d, w, w, d, w, e, e, e, e, e,
  e, e, e, e, e, w, d, w, w, d, w, w, w, e, e, w, w, w, d, w, w, d, w, e, e, e, e, e,
  w, w, w, w, w, w, d, w, w, d, w, e, e, e, e, e, e, w, d, w, w, d, w, w, w, w, w, w,
  w, d, d, d, d, d, d, d, d, d, w, e, e, e, e, e, e, w, d, d, d, d, d, d, d, d, d, w,
  w, w, w, w, w, w, d, w, w, d, w, e, e, e, e, e, e, w, d, w, w, d, w, w, w, w, w, w,
  e, e, e, e, e, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, e, e, e, e, e,
  e, e, e, e, e, w, d, w, w, d, d, d, d, d, d, d, d, d, d, w, w, d, w, e, e, e, e, e,
  e, e, e, e, e, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, e, e, e, e, e,
  w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w,
  w, d, d, d, d, d, d, d, d, d, d, d, d, w, w, d, d, d, d, d, d, d, d, d, d, d, d, w,
  w, d, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, d, w,
  w, d, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, d, w,
  w, d, d, d, w, w, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, w, w, d, d, d, w,
  w, w, w, d, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, d, w, w, w,
  w, w, w, d, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, d, w, w, w,
  w, d, d, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, d, d, w,
  w, d, w, w, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, w, w, d, w,
  w, d, w, w, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, w, w, d, w,
  w, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, w,
  w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w
  ]
  where 
    w = Wall
    d = PacDot
    e = Empty

gameGridWidth :: Int
gameGridWidth = 28
gameGridHeight :: Int
gameGridHeight = 30

initialGameGrid :: Grid
initialGameGrid = Grid gameGridWidth gameGridHeight (parseGrid initialGameTiles gameGridWidth gameGridHeight)

initialState :: GameState
initialState = GameState 
  0                               -- elapsed time
  Playing                         -- play state
  East                            -- direction
  Chase                           -- ghost mode
  initialGameGrid                 -- grid
  (PacMan 3 (Position 1 1) East)  -- pacman
  [Blinky (Position 0 0), Pinky (Position 0 0), Inky (Position 0 0), Clyde (Position 0 0)]

secondsBetweenCycles :: Float
secondsBetweenCycles = 1 / 3

{-
  Game state models
-}
data GameState = GameState { 
  -- name
  elapsedTime :: Float,
  playState :: PlayState,
  nextDirection :: Direction,

  -- name
  ghostMode  :: GhostMode,

  -- name
  grid :: Grid,

  -- name
  player :: Player,
  enemies :: [Enemy]
}

data PlayState = Playing | Paused | Finished

{-
  Name
-}
data Player = PacMan { lives :: Int, posPlayer :: Position, direction :: Direction }
data Enemy = 
  Blinky { posEnemy :: Position } 
  | Pinky { posEnemy :: Position } 
  | Inky { posEnemy :: Position } 
  | Clyde { posEnemy :: Position } 

{-
  Name
-}
data GhostMode = Chase | Scatter | Frightened

{-
  Name
-}
-- Position x y
data Position = Position Int Int deriving (Show)
data Direction = North | East | South | West deriving (Show)  
data Tile = Empty | Wall | PacDot | PacFruit deriving (Show)  
data Grid = Grid { width :: Int,  height :: Int, tiles :: Seq (Tile, Int, Int) }

halfNegativeWindowSizeFromGrid :: Grid -> (Float, Float)
halfNegativeWindowSizeFromGrid (Grid w h _) = (-(fromIntegral w * 15), fromIntegral h * 15) 

tileSize :: Int
tileSize = 30

windowSizeFromGrid :: Grid -> (Int, Int)
windowSizeFromGrid (Grid w h _) = (w * tileSize, h * tileSize) 

parseGrid :: [Tile] -> Int -> Int -> Seq (Tile, Int, Int)
parseGrid tiles width height = fromList (zip3 tiles columnIndexArray rowIndexArray)
  where 
    columnIndexArray = (concat . replicate height) [0 .. width - 1]
    rowIndexArray = concat $ transpose $ replicate width [0 .. height - 1]

getTileFromGrid :: Grid -> Position -> Tile
getTileFromGrid (Grid _ _ tiles) (Position x y) = case lookup (x + gameGridWidth * y) tiles of
  Just (tile, _, _) -> tile
  -- maybe return something else here, since apperently, there is not tile at the given position
  _                 -> Wall

getTileFromTuple :: (Tile, Int, Int) -> Tile
getTileFromTuple (tile, _, _) = tile

getNextPositionFromPlayer :: Player -> Position
getNextPositionFromPlayer player@PacMan{direction = North, posPlayer = (Position x y)} = Position x (y-1)
getNextPositionFromPlayer player@PacMan{direction = East, posPlayer = (Position x y)}  = Position (x+1) y
getNextPositionFromPlayer player@PacMan{direction = South, posPlayer = (Position x y)} = Position x (y+1)
getNextPositionFromPlayer player@PacMan{direction = West, posPlayer = (Position x y)}  = Position (x-1) y

-- updateTileOfGrid :: Grid -> Position -> Tile -> Grid