module Controller.Grid where

import Model
import Model.Grid
import Model.Player
import Settings
import Helpers

gridAfterUpdate :: Grid -> Player -> Grid
gridAfterUpdate grid updatePlayer = case tile of
  PacDot   -> (updateTileOfGrid grid positionAfterUpdate Empty) { dots = dots grid - 1 }
  PacFruit -> updateTileOfGrid grid positionAfterUpdate Empty
  _        -> grid
  where
    positionAfterUpdate = posPlayer updatePlayer
    tile = getTileFromGrid grid positionAfterUpdate