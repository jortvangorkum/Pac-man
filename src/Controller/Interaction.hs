module Controller.Interaction where

import Model
import Settings
import Helpers

-- also passes nextEnemies since otherwise the player and enemy can cross eachother without knowing
interactPlayerWithEnemies :: Player -> Player -> [Enemy] -> Player
interactPlayerWithEnemies player nextPlayer enemies = updatePosition p'
  where 
    p'= player { lives = foldr (checkPlayerAndEnemyPosition nextPlayer) (lives p) enemies }
    p = player { lives = foldr (checkPlayerAndEnemyPosition player) (lives player) enemies }

    updatePosition :: Player -> Player
    updatePosition updatedPlayer
      | lives player /= lives updatedPlayer = updatedPlayer { posPlayer = initialPlayerPosition, dirPlayer = East }
      | otherwise                           = updatedPlayer
      
    -- check if the position is the same, since in that case you are sure they are collided
    checkPlayerAndEnemyPosition :: Player -> Enemy -> Int -> Int
    checkPlayerAndEnemyPosition player enemy lives'
      | lives player /= lives'             = lives'
      | posEnemy enemy == posPlayer player = lives' - 1
      | otherwise                          = lives'


updateScore :: Int -> Tile -> Int
updateScore score tile = case tile of
  PacDot   -> score + 10
  PacFruit -> score + 50
  _        -> score

updateAmountDots :: Int -> Tile -> Int
updateAmountDots dots tile = case tile of
  PacDot -> dots - 1
  _      -> dots

updateGhostMode :: Tile -> GhostMode -> Int -> Int -> GhostMode
updateGhostMode PacFruit _ _ _ = Frightened
updateGhostMode _ ghostMode begin current
  | check     = Chase
  | otherwise = ghostMode
  where
    check = current - begin >= invincibilityCycles && begin > 0

updateInvicibilityBegin :: Tile -> Int -> Int -> Int
updateInvicibilityBegin PacFruit _ current = current
updateInvicibilityBegin _ begin _ = begin