module Helpers where

import Model
import Settings
import Graphics.Gloss

{-
  View helpers
-}
-- first translate from center to top left, then translate from grid index to screen position, then offset by tile size from center to top left
translateToGrid :: Int -> Int -> (Picture -> Picture)
translateToGrid column row = translate 0 (-(fromIntegral topScoreBarSize/2)) . translate width height . translate column' row' . translate (tileSize' / 2) (-tileSize' / 2)
  where
    width = -(fromIntegral (gameGridWidth * tileSize) / 2)
    height = fromIntegral(gameGridHeight * tileSize) / 2
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

extraTranslation :: Int -> Int -> Float -> (Picture -> Picture)
extraTranslation dx dy time = translate dx' dy'
  where
    dx' = fromIntegral dx * extraTranslationAmount
    dy' = fromIntegral dy * extraTranslationAmount
    extraTranslationAmount = (time / secondsBetweenCycles) * fromIntegral tileSize

sizeFromPercentage :: Float -> Float
sizeFromPercentage size = fromIntegral tileSize * size

viewTopBar :: Picture -> Picture
viewTopBar picture = translate 0 (fromIntegral spaceForSides / 1.25) $ translateToGrid 0 0 picture

viewText :: Float -> Picture -> Picture
viewText size picture = scale ((t / 30) * size) ((t / 30) * size) $ color white picture
  where
    t = fromIntegral tileSize

{-
  Model helpers
-}

{-
  Controller helpers
-}