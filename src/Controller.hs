-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Settings
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Char
import Data.Maybe

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- Game Iteration
  | elapsedTime gstate + secs > secondsBetweenCycles = do 
      rdirs <- mapM (`randomDirection` grid gstate) (nextEnemies gstate)
      return $ gstate {
        -- grid
        grid = gridAfterUpdate (grid gstate) (nextPlayer gstate),
        -- movables
        player = nextPlayer gstate, 
        nextPlayer = updatePlayer (nextPlayer gstate) (grid gstate), 
        enemies = nextEnemies gstate,
        nextEnemies = updateEnemies (zip (nextEnemies gstate) rdirs),
        -- time
        elapsedTime = 0
      }
  -- Just update the elapsed time
  | otherwise = 
    return $ gstate { 
      elapsedTime = elapsedTime gstate + secs
    }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = case c of
  'w' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  's' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  'a' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  'd' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }

inputKey (EventKey (SpecialKey s) Down _ _) gstate = case s of
  KeyUp    -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  KeyDown  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  KeyLeft  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  KeyRight -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }

inputKey _ gstate = gstate

direct :: Player -> Direction -> Player
direct player direction' = player { dirPlayer = direction' }

tryDirect :: Player -> Direction -> Grid -> Player
tryDirect player direction' grid = player { intendedDirPlayer  = direction' }

movePlayer :: Player -> Direction -> Player
movePlayer player@PacMan{posPlayer = pos} dir = player { posPlayer = move pos dir} 

move :: Position -> Direction -> Position
move (Position x y) North = Position x (y-1)
move (Position x y) East = Position (x + 1) y
move (Position x y) South = Position x (y+1)
move (Position x y) West = Position (x - 1) y

updatePlayer :: Player -> Grid -> Player
updatePlayer player grid = 
  {-
    the intended direction of the player is checked, 
    since the intended direction was different than what the player is currently moving to,
    if it is possible to move there,
    update the direction of the player
  -}
  case nextTileIntendedDirection of
    (Wall _)   -> checkNextTile
    _          -> movePlayer (direct player playerIntendedDirection) playerIntendedDirection
  where 
    playerDirection = dirPlayer player
    playerIntendedDirection = intendedDirPlayer  player
    nextTile = getNextTileFromPlayer player grid
    nextTileIntendedDirection = getNextTileFromPlayer (direct player playerIntendedDirection) grid
    checkNextTile = case nextTile of
        (Wall _)     -> player
        _        -> movePlayer player (dirPlayer player)

gridAfterUpdate :: Grid -> Player -> Grid
gridAfterUpdate grid updatePlayer = case tile of
  PacDot   -> updateTileOfGrid grid positionAfterUpdate Empty
  PacFruit -> updateTileOfGrid grid positionAfterUpdate Empty
  _        -> grid
  where
    positionAfterUpdate = posPlayer updatePlayer
    tile = getTileFromGrid grid positionAfterUpdate

{-
  Ghosts
-}
getDirections :: Grid -> Position -> [Direction]
getDirections grid (Position x y) = mapMaybe tileToDirection [(north, North), (east, East), (south, South), (west, West)]
  where
    north = getTileFromGrid grid (Position x (y-1))
    east = getTileFromGrid grid (Position (x+1) y) 
    south = getTileFromGrid grid (Position x (y+1))
    west = getTileFromGrid grid (Position (x-1) y) 
    tileToDirection :: (Tile, Direction) -> Maybe Direction
    tileToDirection (tile, dir) = case tile of 
      (Wall _) -> Nothing
      _        -> Just dir

randomDirection :: Enemy -> Grid -> IO Direction
randomDirection enemy grid = case dirs of
    [] -> return (oppositeDirection (dirEnemy enemy))
    _  -> pickElement dirs
    where dirs = filter (\dir -> dir /= oppositeDirection (dirEnemy enemy)) (getDirections grid (posEnemy enemy))

updateEnemies :: [(Enemy, Direction)]-> [Enemy]
updateEnemies enemies = map updateEnemy enemies

updateEnemy :: (Enemy, Direction) -> Enemy
updateEnemy (enemy, rdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }

pickElement :: [a] -> IO a
pickElement [] = error "pickElement: expects a non-empty list"
pickElement [x] = return x
pickElement list = do
    i <- randomRIO (0, length list - 1)
    return $ list !! i

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East