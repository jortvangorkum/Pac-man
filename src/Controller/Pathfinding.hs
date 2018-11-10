module Controller.Pathfinding (pathFinding) where

import Model
import Model.Grid
import Model.Data
import Model.Enemies
import Settings
import Helpers

pathFinding :: Enemy -> Position -> Grid -> Direction
pathFinding enemy target grid
  | length possibleDirections > 1   = getBestDirection possibleDirections current target
  -- we can use head, since we are sure there is an element in the array, and pattern matching is not possible in this case, since it throws an error
  | length possibleDirections == 1  = head possibleDirections
  -- if ghost chases and gets stuck in an alley
  | otherwise                       = oppositeDirection (dirEnemy enemy)
  where
    possibleDirections :: [Direction]
    possibleDirections = filter (\dir -> dir /= oppositeDirection (dirEnemy enemy)) (getDirections grid current)
    current = posEnemy enemy

getBestDirection :: [Direction] -> Position -> Position -> Direction
getBestDirection dirs@(dir:_) current target
  | abs (y2 - y1) > abs (x2 - x1) = checkUpDown True
  | otherwise                     = checkLeftRight True
  where
    x1 = x current
    x2 = x target
    y1 = y current
    y2 = y target
    checkDirection :: [Direction] -> Direction -> Maybe Direction
    checkDirection dirs dir
      | dir `elem` dirs = Just dir
      | otherwise     = Nothing
    checkLeftRight :: Bool -> Direction
    checkLeftRight checkOtherDirection = if x2 - x1 < 0
      then case checkDirection dirs West of 
        Just x -> x
        _ -> if checkOtherDirection then checkUpDown False else dir
      else case checkDirection dirs East of 
        Just x -> x
        _ -> if checkOtherDirection then checkUpDown False else dir
    checkUpDown :: Bool -> Direction
    checkUpDown checkOtherDirection = if y2 - y1 < 0
      then case checkDirection dirs North of
        Just x -> x
        _ -> if checkOtherDirection then checkLeftRight False else dir
      else case checkDirection dirs South of
        Just x -> x
        _ -> if checkOtherDirection then checkLeftRight False else dir


