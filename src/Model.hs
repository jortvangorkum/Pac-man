-- | This module contains the data types
--   which represent the state of the game

{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Data.List


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

initialGameGrid :: Grid
initialGameGrid = Grid 28 30 (parseGrid initialGameTiles 28 30)

initialState :: GameState
initialState = GameState 
  0 
  Playing 
  Chase 
  initialGameGrid
  (PacMan 3 (Position 0 0) North) 
  [Blinky (Position 0 0), Pinky (Position 0 0), Inky (Position 0 0), Clyde (Position 0 0)]

secondsBetweenCycles :: Float
secondsBetweenCycles = 5

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
  enemies :: [Enemy]
}

data PlayState = Playing | Paused | Finished

{-
  Name
-}
data Player = PacMan { lives :: Int, pos :: Position, direction :: Direction }
data Enemy = 
  Blinky { pos :: Position } 
  | Pinky { pos :: Position } 
  | Inky { pos :: Position } 
  | Clyde { pos :: Position } 

{-
  Name
-}
data GhostMode = Chase | Scatter | Frightened

{-
  Name
-}
-- Position x y
data Position = Position Int Int
data Direction = North | East | South | West
data Tile = Empty | Wall | PacDot | PacFruit
data Grid = Grid { width :: Int,  height :: Int, tiles :: [(Tile, Int, Int)] }

halfNegativeWindowSizeFromGrid :: Grid -> (Float, Float)
halfNegativeWindowSizeFromGrid (Grid w h _) = (-(fromIntegral w * 15), fromIntegral h * 15) 

tileSize :: Int
tileSize = 30

windowSizeFromGrid :: Grid -> (Int, Int)
windowSizeFromGrid (Grid w h _) = (w * tileSize, h * tileSize) 

parseGrid :: [Tile] -> Int -> Int -> [(Tile, Int, Int)]
parseGrid tiles width height = zip3 tiles columnIndexArray rowIndexArray
  where 
    columnIndexArray = (concat . replicate height) [0 .. width - 1]
    rowIndexArray = concat $ transpose $ replicate width [0 .. height - 1]