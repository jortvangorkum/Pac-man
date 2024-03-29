module Settings where

import Graphics.Gloss

background :: Color
background = black

frames :: Int
frames = 60

secondsBetweenCycles :: Float
secondsBetweenCycles = 1 / 4

tileSize :: Int
tileSize = 30

topScoreBarSize :: Int
topScoreBarSize = tileSize * 3

spaceForSides :: Int
spaceForSides = tileSize * 3

invincibilityCycles :: Int
invincibilityCycles = 15