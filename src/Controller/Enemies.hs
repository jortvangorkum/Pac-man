module Controller.Enemies where

import Model
import Settings
import Helpers

import System.Random

updateEnemies :: GhostMode -> Int -> [(Enemy, Direction, Direction)] -> [Enemy]
updateEnemies ghostMode cyclesPassed = map (updateEnemy ghostMode cyclesPassed)

updateEnemy :: GhostMode -> Int -> (Enemy, Direction, Direction) -> Enemy
-- specific enemy behavior
-- updateEnemy Chase (enemy@Blinky{}, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) cdir, dirEnemy = cdir }

-- general enemy behavior
updateEnemy Scatter _ (enemy, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }
updateEnemy Chase cyclesPassed (enemy, rdir, cdir)
    -- once every x cycles make a random movement, so that it does not become to hard
    | cyclesPassed `mod` 8 == 0 = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }
    -- chase enemy
    | otherwise                 = enemy { posEnemy = move (posEnemy enemy) cdir, dirEnemy = cdir }
updateEnemy Frightened _ (enemy, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }

randomDirection :: Enemy -> Grid -> IO Direction
randomDirection enemy grid = case dirs of
    [] -> return (oppositeDirection (dirEnemy enemy))
    _  -> pickElement dirs
    where dirs = filter (\dir -> dir /= oppositeDirection (dirEnemy enemy)) (getDirections grid (posEnemy enemy))

pickElement :: [a] -> IO a
pickElement [] = error "pickElement: expects a non-empty list"
pickElement [x] = return x
pickElement list = do
    i <- randomRIO (0, length list - 1)
    return $ list !! i