module Main where

import Controller
import Model
import View

-- Graphics Library: IO Version of Gloss
import Graphics.Gloss.Interface.IO.Game

{-
  Settings for Display
-}

window :: Display
window = InWindow "Pac-Man" (400, 400) (0,0)

background :: Color
background = black

frames :: Int
frames = 10

{-
  Main function
-}

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
