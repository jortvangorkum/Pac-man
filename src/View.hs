-- | This module defines how to turn
--   the game state into a picture
module View (
  module View,
  module View.Player,
  module View.Enemies,
  module View.Grid,
  module View.Info
) where

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
  -- gameplay
  viewPlayer (grid gstate) (player gstate) (nextPlayer gstate) (elapsedTime gstate), 
  viewTiles (grid gstate) ((tiles . grid) gstate), 
  viewEnemies (grid gstate) zippedEnemies (ghostMode gstate) (elapsedTime gstate),

  -- info
  viewHighScores (grid gstate) (highscores gstate) (playState gstate),
  viewScore (grid gstate) (score gstate),
  viewPlaystate (grid gstate) (playState gstate),
  viewLives (grid gstate) (lives (player gstate))
  ]
  where
    zippedEnemies = zip (enemies gstate) (nextEnemies gstate)