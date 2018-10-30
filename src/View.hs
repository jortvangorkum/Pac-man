-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Settings
import Prelude hiding (Right, Left)
import Data.Foldable (toList)
import Data.Sequence hiding (zip3, replicate, Empty, zip, length)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [
  viewPlayer (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles ((tiles . grid) gstate), 
  viewEnemies zippedEnemies (elapsedTime gstate),
  viewScore (score gstate),
  viewPlaystate (playState gstate),
  viewLives (lives (player gstate))
  ]
  where
    zippedEnemies = zip (enemies gstate) (nextEnemies gstate)

-- first translate from center to top left, then translate from grid index to screen position, then offset by tile size from center to top left
translateToGrid :: Int -> Int -> (Picture -> Picture)
translateToGrid column row = translate 0 (-(fromIntegral topScoreBarSize/2)) . translate width height . translate column' row' . translate (tileSize' / 2) (-tileSize' / 2)
  where
    width = -(fromIntegral (gameGridWidth * tileSize) / 2)
    height = fromIntegral(gameGridHeight * tileSize) / 2
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

tileToPicture :: (Tile, Int, Int) -> Picture
tileToPicture (tile, x, y) = translateToGrid x y o
  where
    t = fromIntegral tileSize
    c = case tile of
      (Wall _)  -> color (makeColor (50/255) (35/255) (170/255) 1)
      Empty     -> color black
      PacDot    -> color white
      PacFruit  -> color white
    o = case tile of
      PacDot                                -> c $ circleSolid (t / 8)
      PacFruit                              -> c $ circleSolid (t / 3)
      Empty                                 -> c blank
      (Wall Top)                            -> topWall
      (Wall Right)                          -> rotate 90 topWall
      (Wall Bottom)                         -> rotate 180 topWall
      (Wall Left)                           -> rotate 270 topWall
      (Wall CornerFromBottomToRightOutside) -> bottomRightOutsideWall
      (Wall CornerFromLeftToBottomOutside)  -> rotate 90 bottomRightOutsideWall
      (Wall CornerFromTopToLeftOutside)     -> rotate 180 bottomRightOutsideWall
      (Wall CornerFromRightToTopOutside)    -> rotate 270 bottomRightOutsideWall
      (Wall CornerFromBottomToRightInside)  -> bottomRightInsideWall
      (Wall CornerFromLeftToBottomInside)  -> rotate 90 bottomRightInsideWall
      (Wall CornerFromTopToLeftInside)     -> rotate 180 bottomRightInsideWall
      (Wall CornerFromRightToTopInside)    -> rotate 270 bottomRightInsideWall
      _                                     -> c $ rectangleSolid t t
      where
        topWall = c $ translate 0 (t/4) $ rectangleSolid t (t/2)
        bottomRightOutsideWall = pictures [c $ translate (t/2) (-t/2) $ arcSolid 90 180 t, color black $ translate (t/2) (-t/2) $ arcSolid 90 180 (t/2.2)]
        bottomRightInsideWall = c $ translate (t/2) (-t/2) $ arcSolid 90 180 (t/2.2)

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

viewEnemies :: [(Enemy, Enemy)] -> Float -> Picture
viewEnemies enemies time  = pictures $ map (`viewEnemy` time) enemies

viewEnemy :: (Enemy, Enemy) -> Float -> Picture
viewEnemy (enemy, enemyNext) time = extraTranslation dx dy time $ translateToGrid x' y' $ pictures [
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
    ghostColor = case enemy of
      (Blinky _ _) -> color (makeColor (255/255) 0 0 1)
      (Pinky _ _) -> color (makeColor (255/255) (177/255) (255/255) 1)
      (Inky _ _) -> color (makeColor 0 (255/255) (255/255) 1)
      (Clyde _ _) -> color (makeColor (255/255) (182/255) (50/255) 1)
    -- later do this based on direction
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

viewTopBar :: Picture -> Picture
viewTopBar picture = translate 0 (fromIntegral spaceForSides / 1.25) $ translateToGrid 0 0 picture

viewText :: Picture -> Picture
viewText picture = scale 0.25 0.25 $ color white picture

viewScore :: Int -> Picture
viewScore score = viewTopBar $ viewText $ text ("Score: " ++ show score)

viewPlaystate :: PlayState -> Picture
viewPlaystate playstate = extraTranslationBasedOnText playstate $ translate (fromIntegral (gameGridWidth * tileSize) / 2) 0 $ viewTopBar $ viewText $ (text . show) playstate
  where 
    extraTranslationBasedOnText Playing  = translate (-t * 2.25) 0
    extraTranslationBasedOnText Paused   = translate (-t * 2.25) 0
    extraTranslationBasedOnText Finished = translate (-t * 2.5) 0
    extraTranslationBasedOnText _        = translate 0 0
    t = fromIntegral tileSize

viewLives :: Int -> Picture
viewLives lives = translate (-t * 4.7) 0 $ translate (fromIntegral (gameGridWidth * tileSize)) 0 $ viewTopBar $ viewText $ text ("Lives: " ++ show lives)
  where 
    t = fromIntegral tileSize

viewHighScores :: [Int] -> Picture
viewHighScores []               = viewText $ text "no score"
viewHighScores scores@(score:_) = viewText $ pictures [color black $ rectangleSolid 500 (200 * fromIntegral scoreAmount), translate (-200) (-100 * fromIntegral scoreAmount) scoresAsTextForBox]
    where
      scoreAmount = length scores
      scoresAsText = map (text . show) scores
      zippedScoresAsText = zip scoresAsText [0 .. scoreAmount]
      scoresAsTextForBox = pictures (map (\(text, i) -> translate 0 (fromIntegral i*180) text) zippedScoresAsText)
