-- | This module contains the data types
--   which represent the state of the game

{-# LANGUAGE DuplicateRecordFields #-}

module Model where


{-
  Initialization
-}
initialState :: GameState
initialState = GameState 0 Playing Chase (Grid 60 60 []) (PacMan 3 (Position 0 0)) [Blinky (Position 0 0), Pinky (Position 0 0), Inky (Position 0 0), Clyde (Position 0 0)]

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
data Player = PacMan { lives :: Int, pos :: Position }
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
data Tile = Empty | Wall | Edible
data Grid = Grid { width :: Int,  height :: Int, tiles :: [Tile] }