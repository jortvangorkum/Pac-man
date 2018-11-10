-- | This module defines how the state changes
--   in response to time and user input
module Controller (
  module Controller,
  module Controller.Input,
  module Controller.Player,
  module Controller.Enemies,
  module Controller.Interaction,
  module Controller.Pathfinding,
  module Controller.Grid
) where

-- controllers
import Controller.Input
import Controller.Player
import Controller.Enemies
import Controller.Interaction
import Controller.Pathfinding
import Controller.Grid

import Model
import Helpers
import Settings

import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteFile

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- Game Iteration
  | playState gstate == Initialise = do
      highScoreBytes <- ByteFile.readFile "data/highscores.json"
      return $ gstate { 
        playState = Playing, 
        highscores = fromMaybe [] (decode highScoreBytes :: Maybe [Int])
      }

  -- Save highscores
  | playState gstate == SavingHighscore = do
    savedFile <- ByteFile.writeFile "data/highscores.json" (encode (score gstate : highscores gstate))
    return $ gstate {
      playState = Finished,
      highscores = score gstate : highscores gstate
    }
  
  -- Finished (via savinghighscores)
  | playState gstate == Finished = return gstate
  | lives (player gstate) <= 0 = return $ gstate { playState = SavingHighscore }
  | (dots . grid) gstate <= 0 = return $ gstate { playState = SavingHighscore }

  -- Paused
  | playState gstate == Paused = return gstate

  -- Playing
  | elapsedTime gstate + secs > secondsBetweenCycles && playState gstate == Playing = do 
      -- Random Directions
      rdirs <- mapM (`randomDirection` grid gstate) (nextEnemies gstate)
      return $ gstate {
        -- grid
        grid = gridAfterUpdate (grid gstate) (nextPlayer gstate),
        -- movables
        player = interactPlayerWithEnemies (nextPlayer gstate) (updatePlayer (nextPlayer gstate) (grid gstate)) (nextEnemies gstate), 
        nextPlayer = updatePlayer (interactPlayerWithEnemies (nextPlayer gstate) (updatePlayer (nextPlayer gstate) (grid gstate)) (nextEnemies gstate)) (grid gstate), 
        enemies = nextEnemies gstate,
        nextEnemies = updateEnemies (ghostMode gstate) (zip3 
          (nextEnemies gstate) -- enemies
          rdirs  -- random directions
          (map (\enemy -> pathFinding enemy ((posPlayer . player) gstate) (grid gstate)) (nextEnemies gstate)) -- chase direction
        ),
        -- time
        elapsedTime = 0,
        cyclesPassed = cyclesPassed gstate + 1,
        score = updateScore (score gstate) (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)),
        ghostMode = updateGhostMode (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)) (ghostMode gstate) (invincibilityBegin gstate) (cyclesPassed gstate),
        invincibilityBegin = updateInvicibilityBegin (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)) (invincibilityBegin gstate) (cyclesPassed gstate)
      }

  -- Just update the elapsed time
  | otherwise = 
    return $ gstate { 
      elapsedTime = elapsedTime gstate + secs
    }