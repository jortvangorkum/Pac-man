module Controller.Player (tryDirect, updatePlayer) where

import Model
import Model.Grid
import Model.Data
import Model.Player
import Settings
import Helpers

direct :: Player -> Direction -> Player
direct player direction' = player { dirPlayer = direction' }

tryDirect :: Player -> Direction -> Grid -> Player
tryDirect player direction' grid = player { intendedDirPlayer  = direction' }

movePlayer :: Player -> Direction -> Player
movePlayer player@PacMan{posPlayer = pos} dir = player { posPlayer = move pos dir} 

updatePlayer :: Player -> Grid -> Player
updatePlayer player grid = 
  {-
    the intended direction of the player is checked, 
    since the intended direction was different than what the player is currently moving to,
    if it is possible to move there,
    update the direction of the player
  -}
  case nextTileIntendedDirection of
    (Wall _)   -> checkNextTile
    _          -> movePlayer (direct player playerIntendedDirection) playerIntendedDirection
  where 
    playerDirection = dirPlayer player
    playerIntendedDirection = intendedDirPlayer  player
    nextTile = getNextTileFromPlayer player grid
    nextTileIntendedDirection = getNextTileFromPlayer (direct player playerIntendedDirection) grid
    checkNextTile = case nextTile of
        (Wall _) -> player
        _        -> movePlayer player (dirPlayer player)