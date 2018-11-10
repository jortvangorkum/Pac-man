module Controller.Input (input) where

import Model
import Settings
import Graphics.Gloss.Interface.IO.Game

import Controller.Player

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = case c of
  '1' -> gstate { grid = levelOneGrid }
  '2' -> gstate { grid = levelOneGrid }
  'p' -> gstate { playState = togglePause (playState gstate) }
  'r' -> initialState
  'w' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  's' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  'a' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  'd' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }
  _   -> gstate 

inputKey (EventKey (SpecialKey s) Down _ _) gstate = case s of
  KeyUp    -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  KeyDown  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  KeyLeft  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  KeyRight -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }
  _        -> gstate

inputKey _ gstate = gstate

togglePause :: PlayState -> PlayState
togglePause Playing = Paused
togglePause Paused = Playing
togglePause Finished = Finished