-- | This module contains the data types
--   which represent the state of the game

module Model where

import Settings
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (length, zip3, replicate, Empty)
import Graphics.Gloss

-- models
import Model.Grid
import Model.LevelOne

{-
  Initialization
-}

initialPlayerPosition :: Position
initialPlayerPosition = Position 14 23

initialState :: GameState
initialState = GameState 
  0                                           -- elapsed time
  0                                           -- cycles passed
  Initialise                                  -- play state
  0                                           -- score
  []                                          -- highscores
  Chase                                       -- ghost mode
  0                                           -- invincibilityBegin
  levelOneGrid                                -- grid
  (PacMan 3 initialPlayerPosition East East)  -- pacman
  (PacMan 3 initialPlayerPosition East East)  -- pacman next position
  [Blinky (Position 1 1) East (makeColor (255/255) 0 0 1), Pinky (Position 1 29) West (makeColor (255/255) (177/255) (255/255) 1), Inky (Position 26 1) East (makeColor 0 (255/255) (255/255) 1), Clyde (Position 26 29) West (makeColor (255/255) (182/255) (50/255) 1)]
  [Blinky (Position 1 1) East (makeColor (255/255) 0 0 1), Pinky (Position 1 29) West (makeColor (255/255) (177/255) (255/255) 1), Inky (Position 26 1) East (makeColor 0 (255/255) (255/255) 1), Clyde (Position 26 29) West (makeColor (255/255) (182/255) (50/255) 1)]

{-
  Game state models
-}
data GameState = GameState { 
  -- name
  elapsedTime :: Float,
  cyclesPassed :: Int,
  playState :: PlayState,
  score :: Int,
  highscores :: [Int],

  -- name
  ghostMode  :: GhostMode,
  invincibilityBegin :: Int,

  -- name
  grid :: Grid,

  -- name
  player :: Player,
  nextPlayer :: Player,
  enemies :: [Enemy],
  nextEnemies :: [Enemy]
}

data PlayState = Initialise | Playing | Paused | SavingHighscore | Finished deriving (Show, Eq)

{-
  Name
-}
data Player = PacMan { lives :: Int, posPlayer :: Position, dirPlayer :: Direction, intendedDirPlayer  :: Direction }
data Enemy = 
  Blinky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Pinky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Inky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Clyde { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 

{-
  Name
-}
data GhostMode = Chase | Scatter | Frightened

{-
  Name
-}
data Position = Position { x :: Int, y :: Int } deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)

levelOneGrid :: Grid
levelOneGrid = Grid tiles width height dots where (tiles, width, height, dots) = parseGrid levelOneTiles