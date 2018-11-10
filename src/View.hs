-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Settings
import Prelude hiding (Right, Left)
import Data.List
import Data.Foldable (toList)
import Data.Sequence hiding (zip3, replicate, Empty, zip, length, sort, take)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [
  viewPlayer (grid gstate) (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles (grid gstate), 
  viewEnemies (grid gstate) zippedEnemies (ghostMode gstate) (elapsedTime gstate),
  viewScore (grid gstate) (score gstate),
  viewPlaystate (grid gstate) (playState gstate),
  viewLives (grid gstate) (lives (player gstate)),
  viewHighScores (grid gstate) (highscores gstate) (playState gstate)
  ]
  where
    zippedEnemies = zip (enemies gstate) (nextEnemies gstate)

sizeFromPercentage :: Float -> Float
sizeFromPercentage size = fromIntegral tileSize * size

-- first translate from center to top left, then translate from grid index to screen position, then offset by tile size from center to top left
translateToGrid :: Grid -> Int -> Int -> (Picture -> Picture)
translateToGrid grid column row = translate 0 (-(fromIntegral topScoreBarSize/2)) . translate width' height' . translate column' row' . translate (tileSize' / 2) (-tileSize' / 2)
  where
    width' = -(fromIntegral (width grid * tileSize) / 2)
    height' = fromIntegral(height grid * tileSize) / 2
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

tileToPicture :: Grid -> (Tile, Int, Int) -> Picture
tileToPicture grid (tile, x, y) = translateToGrid grid x y o
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

viewTiles :: Grid -> Picture
viewTiles grid = pictures $ map (tileToPicture grid) (toList (tiles grid))

viewPlayer :: Grid -> Player -> Player -> Float -> Picture
viewPlayer grid player playerNext time = case player of
  PacMan {}  -> viewPacMan grid player playerNext time

viewPacMan :: Grid -> Player -> Player -> Float -> Picture
viewPacMan grid p pNext time = extraTranslation dx dy time $ translateToGrid grid x1 y1 $ rotation $ pictures [
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

viewEnemies :: Grid -> [(Enemy, Enemy)] -> GhostMode -> Float -> Picture
viewEnemies grid enemies ghostMode time  = pictures $ map (viewEnemy grid ghostMode time) enemies

viewEnemy :: Grid -> GhostMode -> Float -> (Enemy, Enemy) -> Picture
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
      Frightened -> color $ makeColor 0 0 (255/255) 1
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

viewTopBar :: Grid -> Picture -> Picture
viewTopBar grid picture = translate 0 (fromIntegral spaceForSides / 1.25) $ translateToGrid grid 0 0 picture

viewText :: Float -> Picture -> Picture
viewText size picture = scale ((t / 30) * size) ((t / 30) * size) $ color white picture
  where
    t = fromIntegral tileSize

viewScore :: Grid -> Int -> Picture
viewScore grid score = viewTopBar grid $ viewText 0.25 $ text ("Score: " ++ show score)

viewPlaystate :: Grid -> PlayState -> Picture
viewPlaystate grid playstate = extraTranslationBasedOnText playstate $ translate (fromIntegral (width grid * tileSize) / 2) 0 $ viewTopBar grid $ viewText 0.25 $ (text . show) playstate
  where 
    extraTranslationBasedOnText Playing  = translate (-t * 2.25) 0
    extraTranslationBasedOnText Paused   = translate (-t * 2.25) 0
    extraTranslationBasedOnText Finished = translate (-t * 2.5) 0
    extraTranslationBasedOnText _        = translate 0 0
    t = fromIntegral tileSize

viewLives :: Grid -> Int -> Picture
viewLives grid lives = translate (-t * 4.7) 0 $ translate (fromIntegral (width grid * tileSize)) 0 $ viewTopBar grid $ viewText 0.25 $ text ("Lives: " ++ show lives)
  where 
    t = fromIntegral tileSize

viewHighScores :: Grid -> [Int] -> PlayState -> Picture
viewHighScores _ [] _                       = blank
viewHighScores grid scores@(score:_) playstate = case playstate of
  Finished -> pictures [color black $ rectangleSolid (fromIntegral (width grid) * sizeFromPercentage 0.5) (sizeFromPercentage 2.5 * (fromIntegral scoreAmount + 1)), translate 0 (- sizeFromPercentage 0.8 * (fromIntegral scoreAmount + 1)) scoresAsTextForBox]
  _        -> blank
  where
    scoresSorted = sort (take 5 scores)
    scoreAmount = length scoresSorted + 1 -- because of the extra highscore element added to the list
    scoresAsText = map (text . show) scoresSorted
    zippedScoresAsText = zip3 scoresAsText (map show scoresSorted) [0 .. scoreAmount] ++ [(translate (sizeFromPercentage 3.3) 0 $ text "Highscores:", "Highscores:", scoreAmount)]
    scoresAsTextForBox = pictures (map (\(text, string, i) -> 
      viewText 0.35 $ 
      translate 
      (fromIntegral (length string) * sizeFromPercentage (-1.35)) 
      (fromIntegral i * sizeFromPercentage 5) text) zippedScoresAsText)
