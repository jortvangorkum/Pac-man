module Controller.Enemies where

import Model
import Model.Grid
import Model.Enemies
import Model.Data
import Settings
import Helpers

import System.Random

updateEnemies :: GhostMode -> [(Enemy, Direction, Direction)]-> [Enemy]
updateEnemies ghostMode = map (updateEnemy ghostMode)

updateEnemy :: GhostMode -> (Enemy, Direction, Direction) -> Enemy
-- specific enemy behavior
-- updateEnemy Chase (enemy@Blinky{}, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) cdir, dirEnemy = cdir }

-- general enemy behavior
updateEnemy Scatter (enemy, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }
updateEnemy Chase (enemy, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) cdir, dirEnemy = cdir }
updateEnemy Frightened (enemy, rdir, cdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }

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