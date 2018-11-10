-- | This module contains the data types
--   which represent the state of the game

module Model where

import Settings
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (length, zip3, replicate, Empty)
import Graphics.Gloss

-- models
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
  levelOneGrid                             -- grid
  (countAmountDots levelOneGrid)           -- amount PacDots
  (PacMan 3 initialPlayerPosition East East)  -- pacman
  (PacMan 3 initialPlayerPosition East East)  -- pacman next position
  [
    Blinky (Position 1 1) East (makeColor (255/255) 0 0 1),
    Pinky (Position 1 29) West (makeColor (255/255) (177/255) (255/255) 1),
    Inky (Position 26 1) East (makeColor 0 (255/255) (255/255) 1),
    Clyde (Position 26 30) West (makeColor (255/255) (182/255) (50/255) 1)
  ]
  [
    Blinky (Position 1 1) East (makeColor (255/255) 0 0 1),
    Pinky (Position 1 29) West (makeColor (255/255) (177/255) (255/255) 1),
    Inky (Position 26 1) East (makeColor 0 (255/255) (255/255) 1),
    Clyde (Position 26 29) West (makeColor (255/255) (182/255) (50/255) 1)
  ]

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
  dots :: Int,

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
data Grid = Grid { width :: Int, height :: Int, tiles :: Seq (Tile, Int, Int) }
data Tile = Empty | Wall WallType | PacDot | PacFruit deriving (Show, Eq)  
data WallType = Full | Top | Right | Bottom | Left 
              | CornerFromBottomToRightOutside | CornerFromLeftToBottomOutside | CornerFromTopToLeftOutside | CornerFromRightToTopOutside 
              | CornerFromBottomToRightInside | CornerFromLeftToBottomInside | CornerFromTopToLeftInside | CornerFromRightToTopInside 
              deriving (Show, Eq)  


parseGrid :: [[Tile]] -> (Seq (Tile, Int, Int), Int, Int)
parseGrid tiles@(first:_) = (fromList (zip3 (concat tiles) columnIndexArray rowIndexArray), width, height)
  where 
    width = length first 
    height = length tiles
    columnIndexArray = (concat . replicate height) [0 .. width - 1]
    rowIndexArray = concat $ transpose $ replicate width [0 .. height - 1]

levelOneTiles :: [[Tile]]
levelOneTiles = [
  [m, t, t, t, t, t, t, t, t, t, t, t, t, n, m, t, t, t, t, t, t, t, t, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, b, b, b, x, d, r, l, d, q, b, b, b, x, d, q, b, b, x, d, r],
  [l, d, r, w, w, l, d, r, w, w, w, l, d, r, l, d, r, w, w, w, l, d, r, w, w, l, d, r],
  [l, f, z, t, t, y, d, z, t, t, t, y, d, z, y, d, z, t, t, t, y, d, z, t, t, y, f, r],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, x, d, q, b, b, b, b, b, b, x, d, q, x, d, q, b, b, x, d, r],
  [l, d, z, t, t, y, d, r, l, d, z, t, t, n, m, t, t, y, d, r, l, d, z, t, t, y, d, r],
  [l, d, d, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, d, d, r],
  [p, b, b, b, b, x, d, r, p, b, b, x, d, r, l, d, q, b, b, o, l, d, q, b, b, b, b, o],
  [e, e, e, e, e, l, d, r, m, t, t, y, d, z, y, d, z, t, t, n, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e],
  [m, t, t, t, t, y, d, z, y, d, d, e, e, e, e, e, e, d, d, z, y, d, z, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, e, e, e, e, e, e, d, d, d, d, d, d, d, d, d, d, r],
  [p, b, b, b, b, x, d, q, x, d, d, e, e, e, e, e, e, d, d, q, x, d, q, b, b, b, b, o],
  [e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, d, d, d, d, d, d, d, d, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, q, b, b, b, b, b, b, x, d, r, l, d, r, e, e, e, e, e],
  [m, t, t, t, t, y, d, z, y, d, z, t, t, n, m, t, t, y, d, z, y, d, z, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, b, b, b, x, d, r, l, d, q, b, b, b, x, d, q, b, b, x, d, r],
  [l, d, z, t, n, l, d, z, t, t, t, y, d, z, y, d, z, t, t, t, y, d, r, m, t, y, d, r],
  [l, f, d, d, r, l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r, l, d, d, f, r],
  [p, b, x, d, r, l, d, q, x, d, q, b, b, b, b, b, b, x, d, q, x, d, r, l, d, q, b, o],
  [m, t, y, d, z, y, d, r, l, d, z, t, t, n, m, t, t, y, d, r, l, d, z, y, d, z, t, n],
  [l, d, d, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, r, l, d, d, d, d, d, d, r],
  [l, d, q, b, b, b, b, o, p, b, b, x, d, r, l, d, q, b, b, o, p, b, b, b, b, x, d, r],
  [l, d, z, t, t, t, t, t, t, t, t, y, d, z, y, d, z, t, t, t, t, t, t, t, t, y, d, r],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [p, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, o]
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

levelOneGrid :: Grid
levelOneGrid = Grid width height tiles 
  where (tiles, width, height) = parseGrid levelOneTiles

countAmountDots :: Grid -> Int
countAmountDots grid = foldr (\(tile, x, y) count -> if tile == PacDot then count + 1 else count) 0 (tiles grid)