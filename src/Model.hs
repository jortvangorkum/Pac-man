-- | This module contains the data types
--   which represent the state of the game

module Model where

import Settings
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (length, zip3, replicate, Empty)
import Graphics.Gloss

-- models
import Model.Data
import Model.Grid
import Model.Player
import Model.Enemies
import Model.LevelOne
import Model.LevelTwo

{-
  Initialization
-}

initialState :: GameState
initialState = GameState 
  0                 -- elapsed time
  0                 -- cycles passed
  Initialise        -- play state
  0                 -- score
  []                -- highscores
  Chase             -- ghost mode
  0                 -- invincibilityBegin
  levelOneGrid      -- grid
  levelOnePlayer    -- pacman
  levelOnePlayer    -- pacman next position
  levelOneEnemies   -- enemies
  levelOneEnemies   -- enemies next position

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


levelOneGrid :: Grid
levelOneGrid = Grid tiles width height dots where (tiles, width, height, dots) = parseGrid levelOneTiles

levelTwoGrid :: Grid
levelTwoGrid = Grid tiles width height dots where (tiles, width, height, dots) = parseGrid levelTwoTiles