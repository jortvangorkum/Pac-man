module Main where

import Controller
import Model
import View
import Settings
import Helpers

-- Graphics Library: IO Version of Gloss
import Graphics.Gloss.Interface.IO.Game

-- window :: Display
-- window = InWindow "Pac-Man" ((\(x,y) -> (x+spaceForSides, y+spaceForSides+topScoreBarSize)) (windowSizeFromGrid levelOneGrid)) (0, 0)

{-
  Main function
-}
main :: IO ()
main = playIO FullScreen       -- Or FullScreen
              background       -- Background color
              frames           -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
