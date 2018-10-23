-- | This module contains the data types
--   which represent the state of the game

module Model where

import Settings
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
  w, f, w, w, w, w, d, w, w, w, w, w, d, w, w, d, w, w, w, w, w, d, w, w, w, w, f, w,
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
  w, f, d, d, w, w, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, w, w, d, d, f, w,
  w, w, w, d, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, d, w, w, w,
  w, w, w, d, w, w, d, w, w, d, w, w, w, w, w, w, w, w, d, w, w, d, w, w, d, w, w, w,
  w, d, d, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, w, w, d, d, d, d, d, d, w,
  w, d, w, w, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, w, w, d, w,
  w, d, w, w, w, w, w, w, w, w, w, w, d, w, w, d, w, w, w, w, w, w, w, w, w, w, d, w,
  w, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, w,
  w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w
  ]
  where 
    w = Wall Full
    d = PacDot
    f = PacFruit
    e = Empty

gameGridWidth :: Int
gameGridWidth = 28
gameGridHeight :: Int
gameGridHeight = 30

initialGameGrid :: Grid
initialGameGrid = Grid gameGridWidth gameGridHeight (parseGrid initialGameTiles gameGridWidth gameGridHeight)

initialState :: GameState
initialState = GameState 
  0                                    -- elapsed time
  Playing                              -- play state
  Chase                                -- ghost mode
  initialGameGrid                      -- grid
  (PacMan 3 (Position 1 1) East East)  -- pacman
  (PacMan 3 (Position 2 1) East East)  -- pacman
  [Blinky (Position 12 13), Pinky (Position 13 13), Inky (Position 14 13), Clyde (Position 15 13)]

{-
  Game state models
-}
data GameState = GameState { 
  -- name
  elapsedTime :: Float,
  playState :: PlayState,

  -- name
  ghostMode  :: GhostMode,

  -- name
  grid :: Grid,

  -- name
  player :: Player,
  nextPlayer :: Player,
  enemies :: [Enemy]
}

data PlayState = Playing | Paused | Finished

{-
  Name
-}
data Player = PacMan { lives :: Int, posPlayer :: Position, dirPlayer :: Direction, intendedDirection :: Direction }
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
data Position = Position { x :: Int, y :: Int } deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)  
data Grid = Grid { width :: Int,  height :: Int, tiles :: Seq (Tile, Int, Int) }

data Tile = Empty | Wall WallType | PacDot | PacFruit deriving (Show, Eq)  
data WallType = Full | Top | Right | Bottom | Left | CornerFromBottomToRightOutside | CornerFromLeftToBottomOutside | CornerFromTopToLeftOutside | CornerFromRightToTopOutside deriving (Show, Eq)  


indexFromPosition :: Position -> Int
indexFromPosition (Position x y) = x + gameGridWidth * y

halfNegativeWindowSizeFromGrid :: Grid -> (Float, Float)
halfNegativeWindowSizeFromGrid (Grid w h _) = (-(fromIntegral w * 15), fromIntegral h * 15) 

windowSizeFromGrid :: Grid -> (Int, Int)
windowSizeFromGrid (Grid w h _) = (w * tileSize, h * tileSize) 

parseGrid :: [Tile] -> Int -> Int -> Seq (Tile, Int, Int)
parseGrid tiles width height = fromList (zip3 tiles columnIndexArray rowIndexArray)
  where 
    columnIndexArray = (concat . replicate height) [0 .. width - 1]
    rowIndexArray = concat $ transpose $ replicate width [0 .. height - 1]

getTileFromGrid :: Grid -> Position -> Tile
getTileFromGrid (Grid _ _ tiles) position = case lookup (indexFromPosition position) tiles of
  Just (tile, _, _) -> tile
  _                 -> error "Position is outside of the Grid"

getTileFromTuple :: (Tile, Int, Int) -> Tile
getTileFromTuple (tile, _, _) = tile

getNextTileFromPlayer :: Player -> Grid -> Tile
getNextTileFromPlayer player grid = getTileFromGrid grid (getNextPositionFromPlayer player)

getNextPositionFromPlayer :: Player -> Position
getNextPositionFromPlayer player@PacMan{dirPlayer = North, posPlayer = (Position x y)} = Position x (y-1)
getNextPositionFromPlayer player@PacMan{dirPlayer = East, posPlayer = (Position x y)}  = Position (x+1) y
getNextPositionFromPlayer player@PacMan{dirPlayer = South, posPlayer = (Position x y)} = Position x (y+1)
getNextPositionFromPlayer player@PacMan{dirPlayer = West, posPlayer = (Position x y)}  = Position (x-1) y

updateTileOfGrid :: Grid -> Position -> Tile -> Grid
updateTileOfGrid grid position@(Position x y) tile = grid { tiles = update (indexFromPosition position) (tile, x, y) (tiles grid) }