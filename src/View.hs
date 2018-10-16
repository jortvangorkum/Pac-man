-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

translateToGrid :: Int -> Int -> (Picture -> Picture)
translateToGrid column row = translate column' row' . translate width height
  where
    translateToUpperLeft = translate width height
    width = fromIntegral (-(gameGridWidth `div` 2))
    height = fromIntegral (gameGridHeight `div` 2)
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

view :: GameState -> IO Picture
view = return . viewPure

tileToPicture :: (Tile, Int, Int) -> Picture
tileToPicture (tile, x, y) = translate t' (-t') $ translateToGrid x y $ c o
  where
    t = fromIntegral tileSize
    t' = t / 2
    c = case tile of
      Wall      -> color red
      Empty     -> color black
      PacDot    -> color white
      PacFruit  -> color blue
    o = case tile of
      PacDot    -> circleSolid (t / 8)
      _         -> rectangleSolid t t


viewPure :: GameState -> Picture
viewPure gstate = pictures [viewTiles gstate, viewPlayer gstate]

viewTiles :: GameState -> Picture
viewTiles gstate = pictures picturesFromTiles
  where
    picturesFromTiles = map tileToPicture tilesFromGameState
    tilesFromGameState = (tiles . grid) gstate

viewPlayer :: GameState -> Picture
viewPlayer gstate = case playerFromGameState of
  PacMan {}  -> viewPacMan playerFromGameState
  _       -> error "Player type not found."
  where
    playerFromGameState = player gstate

viewPacMan :: Player -> Picture
viewPacMan (PacMan _ position@(Position x y) direction) = translateToGrid x y $ color yellow $ circleSolid size
  where
    size = fromIntegral tileSize / 2