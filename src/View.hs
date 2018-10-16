-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

tileToPicture :: (Tile, Int, Int) -> Picture
tileToPicture (tile, _x, _y) = translate t' (-t') $ translate x y $ c $ o
  where
    x = fromIntegral _x * t
    y = -(fromIntegral _y * t)
    t = fromIntegral tileSize
    t' = t / 2
    c = case tile of
      Wall      -> color red
      Empty     -> color black
      PacDot    -> color yellow
      PacFruit  -> color blue
    o = case tile of
      PacDot    -> circleSolid (t / 8)
      _         -> rectangleSolid t t


viewPure :: GameState -> Picture
viewPure gstate = pictures (map translateToUpperLeft picturesFromTiles)
  where
    translateToUpperLeft = uncurry translate (halfNegativeWindowSizeFromGrid initialGameGrid)
    picturesFromTiles = map tileToPicture tilesFromGameState
    tilesFromGameState = (tiles . grid) gstate
