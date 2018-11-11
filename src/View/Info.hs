module View.Info (viewScore, viewPlaystate, viewLives, viewHighScores) where

import Model
import Settings
import Helpers
import Graphics.Gloss
import Data.Ord

import Data.List

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
viewHighScores _ [] _                          = blank
viewHighScores grid scores@(score:_) playstate = case playstate of
  Finished -> pictures [color black $ rectangleSolid (fromIntegral (width grid) * sizeFromPercentage 0.5) (sizeFromPercentage 2.5 * (fromIntegral scoreAmount + 1)), translate 0 (- sizeFromPercentage 0.8 * (fromIntegral scoreAmount + 1)) scoresAsTextForBox]
  _        -> blank
  where
    scoresSorted = reverse $ take 5 $ sortOn Down scores
    scoreAmount = length scoresSorted + 1 -- because of the extra highscore element added to the list
    scoresAsText = map (text . show) scoresSorted
    zippedScoresAsText = zip3 scoresAsText (map show scoresSorted) [0 .. scoreAmount] ++ [(translate (sizeFromPercentage 3.3) 0 $ text "Highscores:", "Highscores:", scoreAmount)]
    scoresAsTextForBox = pictures (map (\(text, string, i) -> 
      viewText 0.35 $ 
      translate 
      (fromIntegral (length string) * sizeFromPercentage (-1.35)) 
      (fromIntegral i * sizeFromPercentage 5) text) zippedScoresAsText)