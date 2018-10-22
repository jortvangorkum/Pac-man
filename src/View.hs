-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Data.Foldable (toList)
import Data.Sequence hiding (zip3, replicate, Empty)

-- first translate from center to top left, then translate from grid index to screen position, then offset by tile size from center to top left
translateToGrid :: Int -> Int -> (Picture -> Picture)
translateToGrid column row = translate width height . translate column' row' . translate (tileSize' / 2) (-tileSize' / 2)
  where
    width = -(fromIntegral (gameGridWidth * tileSize) / 2)
    height = fromIntegral(gameGridHeight * tileSize) / 2
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

view :: GameState -> IO Picture
view = return . viewPure

tileToPicture :: (Tile, Int, Int) -> Picture
tileToPicture (tile, x, y) = translateToGrid x y $ c o
  where
    t = fromIntegral tileSize
    c = case tile of
      Wall      -> color red
      Empty     -> color black
      PacDot    -> color white
      PacFruit  -> color white
    o = case tile of
      PacDot    -> circleSolid (t / 8)
      PacFruit  -> circleSolid (t / 3)
      _         -> rectangleSolid t t


viewPure :: GameState -> Picture
viewPure gstate = pictures [viewTiles ((tiles . grid) gstate), viewPlayer (player gstate) (grid gstate) (elapsedTime gstate), viewEnemies (enemies gstate)]

viewTiles :: Seq (Tile, Int, Int) -> Picture
viewTiles tiles = pictures $ map tileToPicture (toList tiles)

viewPlayer :: Player -> Grid -> Float -> Picture
viewPlayer player grid time = case player of
  PacMan {}  -> viewPacMan player grid time

viewPacMan :: Player -> Grid -> Float -> Picture
viewPacMan p@(PacMan _ position@(Position x y) _ _) grid time = extraTranslation (direction nextPlayerIteration) $ translateToGrid x y $ color yellow $ circleSolid size
  where
    size = fromIntegral tileSize / 2
    nextPlayerIteration = tryMove p grid
    -- nextPlayerIterationDirection :: Direction
    -- nextPlayerIterationDirection = direction nextPlayerIteration
    extraTranslationAmount = (time / secondsBetweenCycles) * fromIntegral tileSize
    extraTranslation North = translate 0 extraTranslationAmount
    extraTranslation East = translate extraTranslationAmount 0
    extraTranslation South = translate 0 (-extraTranslationAmount)
    extraTranslation West = translate (-extraTranslationAmount) 0


viewEnemies :: [Enemy] -> Picture
viewEnemies enemies = pictures $ map viewEnemy enemies

viewEnemy :: Enemy -> Picture
viewEnemy enemy = translateToGrid x y $ ghostColor $ circleSolid size 
  where
    (Position x y) = posEnemy enemy
    size = fromIntegral tileSize / 2
    ghostColor = case enemy of
      (Blinky _) -> color red
      (Pinky _) -> color white
      (Inky _) -> color blue
      (Clyde _) -> color orange