-- | This module contains the data types
--   which represent the state of the game

{-# LANGUAGE DuplicateRecordFields #-}

module Model where


{-
  Initialization
-}
initialGameGrid :: Grid
initialGameGrid = Grid 28 31 [
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
  d, d, d, d, d, d, d, d, d, d, w, e, e, e, e, e, e, w, d, d, d, d, d, d, d, d, d, d,
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
    d = Edible
    e = Empty


initialState :: GameState
initialState = GameState 0 Playing Chase initialGameGrid (PacMan 3 (Position 0 0) North) [Blinky (Position 0 0), Pinky (Position 0 0), Inky (Position 0 0), Clyde (Position 0 0)]

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
data Edible = PacDot | PacFruit

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
data Tile = Empty | Wall | Edible 
data Grid = Grid { width :: Int,  height :: Int, tiles :: [Tile] }

windowSizeFromGrid :: Grid -> (Int, Int)
windowSizeFromGrid (Grid w h _) = (w * 30, h * 30) 