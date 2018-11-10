-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Helpers
import Controller
import Settings

-- views
import View.Player
import View.Enemies
import View.Tile
import View.Info

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [
  viewPlayer (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles ((tiles . grid) gstate), 
  viewEnemies zippedEnemies (ghostMode gstate) (elapsedTime gstate),
  viewScore (score gstate),
  viewPlaystate (playState gstate),
  viewLives (lives (player gstate)),
  viewHighScores (highscores gstate) (playState gstate)
  ]
  where
    zippedEnemies = zip (enemies gstate) (nextEnemies gstate)