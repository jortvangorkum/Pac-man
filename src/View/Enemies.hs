module View.Enemies (viewEnemies) where

import Model
import Model.Grid
import Model.Data
import Model.Enemies
import Settings
import Helpers
import Graphics.Gloss

viewEnemies :: Grid -> [(Enemy, Enemy)] -> GhostMode -> Float -> Picture
viewEnemies grid enemies ghostMode time  = pictures $ map (viewEnemy grid ghostMode time) enemies

viewEnemy :: Grid ->  GhostMode -> Float -> (Enemy, Enemy) -> Picture
viewEnemy grid ghostMode time (enemy, enemyNext) = extraTranslation dx dy time $ translateToGrid grid x' y' $ pictures [
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
    (Position x' y') = posEnemy enemy
    size = fromIntegral tileSize / 2 
    ghostColor = case ghostMode of 
      Scatter -> color $ makeColor 0 0 (255/255) 1
      _ -> color $ colEnemy enemy
    eyeDirectionTranslation = case dirEnemy enemyNext of 
      North -> translate 0 (size/5) 
      East -> translate (size/5) 0 
      South -> translate 0 (-size/5)
      West -> translate (-size/5) 0 
    x1 = (x . posEnemy) enemy
    x2 = (x . posEnemy) enemyNext
    y1 = (y . posEnemy) enemy
    y2 = (y . posEnemy) enemyNext
    dx =  x2 - x1
    dy = -(y2 - y1)