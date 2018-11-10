-- | This module defines how to turn
--   the game state into a picture
module View where

-- views
import View.Player
import View.Enemies
import View.Grid
import View.Info

import Graphics.Gloss
import Model
import Helpers
import Controller
import Settings


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [
  viewPlayer (grid gstate) (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles (grid gstate) ((tiles . grid) gstate), 
  viewEnemies (grid gstate) zippedEnemies (ghostMode gstate) (elapsedTime gstate),
  viewScore (grid gstate) (score gstate),
  viewPlaystate (grid gstate) (playState gstate),
  viewLives (grid gstate) (lives (player gstate)),
  viewHighScores (grid gstate) (highscores gstate) (playState gstate)
  ]
  where
    zippedEnemies = zip (enemies gstate) (nextEnemies gstate)