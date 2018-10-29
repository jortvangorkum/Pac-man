module Main where

import Controller
import Model
import View
import Settings

-- Graphics Library: IO Version of Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Pac-Man" ((\(x,y) -> (x+spaceForSides, y+spaceForSides+topScoreBarSize)) (windowSizeFromGrid initialGameGrid)) (0, 0)

{-
  Main function
-}
main :: IO ()
main = playIO window           -- Or FullScreen
              background       -- Background color
              frames           -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
