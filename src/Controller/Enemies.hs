module Controller.Enemies where

import Model
import Settings
import Helpers

import System.Random

updateEnemies :: [(Enemy, Direction)]-> [Enemy]
updateEnemies = map updateEnemy

updateEnemy :: (Enemy, Direction) -> Enemy
updateEnemy (enemy, rdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }

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