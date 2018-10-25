-- | This module contains the data types
--   which represent the state of the game

module Model where

import Settings
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (zip3, replicate, Empty)


{-
  Initialization
-}
initialGameTiles :: [Tile]
initialGameTiles = [
  m, t, t, t, t, t, t, t, t, t, t, t, t, n, m, t, t, t, t, t, t, t, t, t, t, t, t, n,
  l, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, r,
  l, d, q, b, b, x, d, q, b, b, b, x, d, r, l, d, q, b, b, b, x, d, q, b, b, x, d, r,
  l, d, r, w, w, l, d, r, w, w, w, l, d, r, l, d, r, w, w, w, l, d, r, w, w, l, d, r,
  l, f, z, t, t, y, d, z, t, t, t, y, d, z, y, d, z, t, t, t, y, d, z, t, t, y, f, r,
  l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r,
  l, d, q, b, b, x, d, q, x, d, q, b, b, b, b, b, b, x, d, q, x, d, q, b, b, x, d, r,
  l, d, z, t, t, y, d, r, l, d, z, t, t, n, m, t, t, y, d, r, l, d, z, t, t, y, d, r,
  l, d, d, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, d, d, r,
  p, b, b, b, b, x, d, r, p, b, b, x, d, r, l, d, q, b, b, o, l, d, q, b, b, b, b, o,
  e, e, e, e, e, l, d, r, m, t, t, y, d, z, y, d, z, t, t, n, l, d, r, e, e, e, e, e,
  e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e,
  e, e, e, e, e, l, d, r, l, d, q, b, b, e, e, b, b, x, d, r, l, d, r, e, e, e, e, e,
  m, t, t, t, t, y, d, z, y, d, r, e, e, e, e, e, e, l, d, z, y, d, z, t, t, t, t, n,
  l, d, d, d, d, d, d, d, d, d, r, e, e, e, e, e, e, l, d, d, d, d, d, d, d, d, d, r,
  p, b, b, b, b, x, d, q, x, d, r, e, e, e, e, e, e, l, d, q, x, d, q, b, b, b, b, o,
  e, e, e, e, e, l, d, r, l, d, z, t, t, t, t, t, t, y, d, r, l, d, r, e, e, e, e, e,
  e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e,
  e, e, e, e, e, l, d, r, l, d, q, b, b, b, b, b, b, x, d, r, l, d, r, e, e, e, e, e,
  m, t, t, t, t, y, d, z, y, d, z, t, t, n, m, t, t, y, d, z, y, d, z, t, t, t, t, n,
  l, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, r,
  l, d, q, b, b, x, d, q, b, b, b, x, d, r, l, d, q, b, b, b, x, d, q, b, b, x, d, r,
  l, d, z, t, n, l, d, z, t, t, t, y, d, z, y, d, z, t, t, t, y, d, r, m, t, y, d, r,
  l, f, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, f, r,
  p, b, x, d, r, l, d, q, x, d, q, b, b, b, b, b, b, x, d, q, x, d, r, l, d, q, b, o,
  m, t, y, d, z, y, d, r, l, d, z, t, t, n, m, t, t, y, d, r, l, d, z, y, d, z, t, n,
  l, d, d, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, d, d, r,
  l, d, q, b, b, b, b, o, p, b, b, x, d, r, l, d, q, b, b, o, p, b, b, b, b, x, d, r,
  l, d, z, t, t, t, t, t, t, t, t, y, d, z, y, d, z, t, t, t, t, t, t, t, t, y, d, r,
  l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r,
  p, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, o
  ]
  where 
    w = Wall Full
    d = PacDot
    f = PacFruit
    e = Empty
    t = Wall Top
    r = Wall Right
    b = Wall Bottom
    l = Wall Left

    m = Wall CornerFromBottomToRightOutside
    n = Wall CornerFromLeftToBottomOutside
    o = Wall CornerFromTopToLeftOutside
    p = Wall CornerFromRightToTopOutside

    q = Wall CornerFromBottomToRightInside
    x = Wall CornerFromLeftToBottomInside
    y = Wall CornerFromTopToLeftInside
    z = Wall CornerFromRightToTopInside

gameGridWidth :: Int
gameGridWidth = 28
gameGridHeight :: Int
gameGridHeight = 31

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
  [Blinky (Position 12 13) East, Pinky (Position 13 13) East, Inky (Position 14 13) East, Clyde (Position 15 13) East]
  [Blinky (Position 12 13) East, Pinky (Position 13 13) East, Inky (Position 14 13) East, Clyde (Position 15 13) East]

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
  enemies :: [Enemy],
  nextEnemies :: [Enemy]
}

data PlayState = Playing | Paused | Finished

{-
  Name
-}
data Player = PacMan { lives :: Int, posPlayer :: Position, dirPlayer :: Direction, intendedDirPlayer  :: Direction }
data Enemy = 
  Blinky { posEnemy :: Position, dirEnemy :: Direction } 
  | Pinky { posEnemy :: Position, dirEnemy :: Direction } 
  | Inky { posEnemy :: Position, dirEnemy :: Direction } 
  | Clyde { posEnemy :: Position, dirEnemy :: Direction } 

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
data WallType = Full | Top | Right | Bottom | Left 
              | CornerFromBottomToRightOutside | CornerFromLeftToBottomOutside | CornerFromTopToLeftOutside | CornerFromRightToTopOutside 
              | CornerFromBottomToRightInside | CornerFromLeftToBottomInside | CornerFromTopToLeftInside | CornerFromRightToTopInside 
              deriving (Show, Eq)  


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