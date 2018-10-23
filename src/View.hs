-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Settings
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
      (Wall _)  -> color red
      Empty     -> color black
      PacDot    -> color white
      PacFruit  -> color white
    o = case tile of
      PacDot    -> circleSolid (t / 8)
      PacFruit  -> circleSolid (t / 3)
      Empty     -> blank
      _         -> rectangleSolid t t


viewPure :: GameState -> Picture
viewPure gstate = pictures [
  viewPlayer (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles ((tiles . grid) gstate), 
  viewEnemies (enemies gstate)
  ]

extraTranslation :: Int -> Int -> Float -> (Picture -> Picture)
extraTranslation dx dy time = translate dx' dy'
  where
    dx' = fromIntegral dx * extraTranslationAmount
    dy' = fromIntegral dy * extraTranslationAmount
    extraTranslationAmount = (time / secondsBetweenCycles) * fromIntegral tileSize

viewTiles :: Seq (Tile, Int, Int) -> Picture
viewTiles tiles = pictures $ map tileToPicture (toList tiles)

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
    rotation = case direction pNext of
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

viewEnemies :: [Enemy] -> Picture
viewEnemies enemies = pictures $ map viewEnemy enemies

viewEnemy :: Enemy -> Picture
viewEnemy enemy = translateToGrid x y $ pictures [
  
    -- body 
    ghostColor $ arcSolid 0 180 size,
    translate 0 (-size/3) $ ghostColor $ rectangleSolid (fromIntegral tileSize) size,
    translate 0 (-size/1.5) $ rotate 180 $ ghostColor $ arcSolid 180 0 (size/3),
    translate (size/1.5) (-size/1.5) $ rotate 180 $ ghostColor $ arcSolid 180 0 (size/3),
    translate (-size/1.5) (-size/1.5) $ rotate 180 $ ghostColor $ arcSolid 180 0 (size/3),
  
    -- eyes
    pictures [
      -- white
      translate (-size/2.6) (size/3) $ color white $ circleSolid (size/3),
      translate (size/2.6) (size/3) $ color white $ circleSolid (size/3),
      -- blue
      eyeDirectionTranslation $ translate (-size/2.6) (size/3) $ color blue $ circleSolid (size/5),
      eyeDirectionTranslation $ translate (size/2.6) (size/3) $ color blue $ circleSolid (size/5)
    ]
  ]
  where
    (Position x y) = posEnemy enemy
    size = fromIntegral tileSize / 2
    ghostColor = case enemy of
      (Blinky _) -> color (makeColor (255/255) 0 0 1)
      (Pinky _) -> color (makeColor (255/255) (177/255) (255/255) 1)
      (Inky _) -> color (makeColor 0 (255/255) (255/255) 1)
      (Clyde _) -> color (makeColor (255/255) (182/255) (50/255) 1)
    -- later do this based on direction
    eyeDirectionTranslation = translate (size/5) 0 