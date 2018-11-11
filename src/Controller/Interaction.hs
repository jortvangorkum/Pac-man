module Controller.Interaction where

import Model
import Settings
import Helpers

interactPlayerWithEnemies :: Player -> Player -> [Enemy] -> GhostMode -> Player
interactPlayerWithEnemies player nextPlayer enemies ghostMode
  | collided = case ghostMode of 
    Frightened -> player
    _          -> player { lives = lives player - 1, posPlayer = initPosPlayer player }
  | otherwise   = player
  where
    collided = any (playerCollidedWithEnemy player nextPlayer) enemies

interactEnemiesWithPlayer :: Player -> Player -> [Enemy] -> GhostMode -> [Enemy]
interactEnemiesWithPlayer player nextPlayer enemies ghostMode = map enemyCollidedWithPlayer enemies
  where
    enemyCollidedWithPlayer :: Enemy -> Enemy    
    enemyCollidedWithPlayer enemy 
      | collided = case ghostMode of
        Frightened -> enemy { posEnemy = initPosEnemy enemy }
        _          -> enemy
      | otherwise   = enemy
      where
        collided = playerCollidedWithEnemy player nextPlayer enemy

-- also passes nextEnemies since otherwise the player and enemy can cross eachother without knowing
playerCollidedWithEnemy :: Player -> Player -> Enemy -> Bool
playerCollidedWithEnemy player nextPlayer enemy = checkPlayerAndEnemyPosition player enemy || checkPlayerAndEnemyPosition nextPlayer enemy
  where 
    checkPlayerAndEnemyPosition :: Player -> Enemy -> Bool
    checkPlayerAndEnemyPosition player enemy = posEnemy enemy == posPlayer player


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