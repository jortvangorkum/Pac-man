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
step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = case c of
  'w' -> gstate { player = move North (player gstate) }
  's' -> gstate { player = move South (player gstate) }
  'a' -> gstate { player = move West (player gstate) }
  'd' -> gstate { player = move East (player gstate) }
inputKey _ gstate = gstate

move :: Direction -> Player -> Player
move North player@(PacMan {posPlayer = (Position x y)}) = player { posPlayer = Position x (y-1) } 
move East player@(PacMan {posPlayer = (Position x y)}) = player { posPlayer = Position (x + 1) y } 
move South player@(PacMan {posPlayer = (Position x y)}) = player { posPlayer = Position x (y+1) } 
move West player@(PacMan {posPlayer = (Position x y)}) = player { posPlayer = Position (x - 1) y } 