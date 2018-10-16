-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Char

-- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs }

step :: Float -> GameState -> IO GameState
step secs gstate
  -- Game Iteration
  | elapsedTime gstate + secs > secondsBetweenCycles =
    do 
      -- print "Current: "
      -- print (posPlayer (player gstate))
      -- print (getTileFromGrid (grid gstate) (posPlayer (player gstate)))
      -- print "Next: "
      -- print (getNextPositionFromPlayer (player gstate))
      -- print (getTileFromGrid (grid gstate) (getNextPositionFromPlayer (player gstate)))
      return $ gstate {player = tryMove (player gstate) (grid gstate), elapsedTime = 0 }
  -- Just update the elapsed time
  | otherwise = 
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = case c of
  'w' -> gstate { player = direct North (player gstate) }
  's' -> gstate { player = direct South (player gstate) }
  'a' -> gstate { player = direct West (player gstate) }
  'd' -> gstate { player = direct East (player gstate) }

inputKey (EventKey (SpecialKey s) Down _ _) gstate = case s of
  KeyUp    -> gstate { player = direct North (player gstate) }
  KeyDown  -> gstate { player = direct South (player gstate) }
  KeyLeft  -> gstate { player = direct West (player gstate) }
  KeyRight -> gstate { player = direct East (player gstate) }

inputKey _ gstate = gstate

direct :: Direction -> Player -> Player
direct direction' player = player { direction = direction' }

move :: Player -> Direction -> Player
move player@PacMan{posPlayer = (Position x y)} North = player { posPlayer = Position x (y-1) } 
move player@PacMan{posPlayer = (Position x y)} East = player { posPlayer = Position (x + 1) y } 
move player@PacMan{posPlayer = (Position x y)} South = player { posPlayer = Position x (y+1) } 
move player@PacMan{posPlayer = (Position x y)} West = player { posPlayer = Position (x - 1) y } 

tryMove :: Player -> Grid -> Player
tryMove player grid = case nextTile of
  PacDot -> move player (direction player)
  Wall -> player
  where 
    nextTile = getTileFromGrid grid (getNextPositionFromPlayer player)

  