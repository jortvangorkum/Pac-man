module View.Player (viewPlayer) where

import Model
import Settings
import Helpers
import Graphics.Gloss

viewPlayer :: Player -> Player -> Float -> Picture
viewPlayer player playerNext time = case player of
  PacMan {}  -> viewPacMan player playerNext time

viewPacMan :: Player -> Player -> Float -> Picture
viewPacMan p pNext time = extraTranslation dx dy time $ translateToGrid x1 y1 $ rotation $ pictures [
  color yellow $ circleSolid size, 
  color black $ arcSolid (20 + (amount * 70)) (160 - (amount * 70)) (size + 1)
  ]
  where
    amount = abs (((time / secondsBetweenCycles) - 0.5) * 2)
    rotation = case dirPlayer pNext of
      North -> rotate 0
      East  -> rotate 90
      South -> rotate 180
      West  -> rotate 270
    x1 = (x . posPlayer) p
    x2 = (x . posPlayer) pNext
    y1 = (y . posPlayer) p
    y2 = (y . posPlayer) pNext
    dx =  x2 - x1
    dy = -(y2 - y1)
    size = fromIntegral tileSize / 2