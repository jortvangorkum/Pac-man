-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Char

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- Game Iteration
  | elapsedTime gstate + secs > secondsBetweenCycles =
    return $ gstate {
      grid = gridByNextMove (grid gstate) (player gstate),
      player = tryMove (player gstate) (grid gstate), 
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
  'w' -> gstate { player = tryDirect (player gstate) North (grid gstate) }
  's' -> gstate { player = tryDirect (player gstate) South (grid gstate) }
  'a' -> gstate { player = tryDirect (player gstate) West (grid gstate) }
  'd' -> gstate { player = tryDirect (player gstate) East (grid gstate) }

inputKey (EventKey (SpecialKey s) Down _ _) gstate = case s of
  KeyUp    -> gstate { player = tryDirect (player gstate) North (grid gstate) }
  KeyDown  -> gstate { player = tryDirect (player gstate) South (grid gstate) }
  KeyLeft  -> gstate { player = tryDirect (player gstate) West (grid gstate) }
  KeyRight -> gstate { player = tryDirect (player gstate) East (grid gstate) }

inputKey _ gstate = gstate

direct :: Player -> Direction -> Player
direct player direction' = player { direction = direction' }

tryDirect :: Player -> Direction -> Grid -> Player
tryDirect player direction' grid = case nextTile of
  PacDot   -> playerNewDirection
  PacFruit -> playerNewDirection
  Empty    -> playerNewDirection
  Wall     -> playerWithUpdatedIntendedDirection
  where
    playerWithUpdatedIntendedDirection = player { intendedDirection = direction' }
    playerNewDirection = direct playerWithUpdatedIntendedDirection direction' 
    nextTile = getTileFromGrid grid (getNextPositionFromPlayer playerNewDirection)

move :: Player -> Direction -> Player
move player@PacMan{posPlayer = (Position x y)} North = player { posPlayer = Position x (y-1) } 
move player@PacMan{posPlayer = (Position x y)} East = player { posPlayer = Position (x + 1) y } 
move player@PacMan{posPlayer = (Position x y)} South = player { posPlayer = Position x (y+1) } 
move player@PacMan{posPlayer = (Position x y)} West = player { posPlayer = Position (x - 1) y } 

tryMove :: Player -> Grid -> Player
tryMove player grid = 
  {-
    the intended direction of the player is checked, 
    since the intended direction was different than what the player is currently moving to,
    if it is possible to move there,
    update the direction of the player
  -}
  case nextTileIntendedDirection of
    PacDot   -> move (direct player playerIntendedDirection) playerIntendedDirection
    PacFruit -> move (direct player playerIntendedDirection) playerIntendedDirection
    Empty    -> move (direct player playerIntendedDirection) playerIntendedDirection
    _        -> case nextTile of
        PacDot   -> move player (direction player)
        PacFruit -> move player (direction player)
        Empty    -> move player (direction player)
        _        -> player
  where 
    playerDirection = direction player
    playerIntendedDirection = intendedDirection player
    nextTile = getTileFromGrid grid (getNextPositionFromPlayer player)
    nextTileIntendedDirection = getTileFromGrid grid (getNextPositionFromPlayer (direct player playerIntendedDirection))

getNextPositionFromPlayerByIntention :: Player -> Grid -> Position
getNextPositionFromPlayerByIntention player grid = posPlayer (tryMove player grid)

gridByNextMove :: Grid -> Player -> Grid
gridByNextMove grid player = case nextTile of
  PacDot   -> updateTileOfGrid grid nextPosition Empty
  PacFruit -> updateTileOfGrid grid nextPosition Empty
  _        -> grid
  where
    nextPosition = getNextPositionFromPlayerByIntention player grid
    nextTile = getTileFromGrid grid nextPosition

  