module Main where

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
main = playIO window background frames
              undefined       -- Initial state
              undefined       -- View function
              undefined       -- Event function
              undefined       -- Step function
