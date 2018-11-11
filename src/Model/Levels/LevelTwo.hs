module Model.Levels.LevelTwo where

import Model.Grid
import Model.Data
import Model.Enemies
import Model.Player

import Graphics.Gloss
import Prelude hiding (lookup, zip3, Right, Left)

levelTwoPlayer = PacMan 3 (Position 14 23) (Position 14 23) East East
levelTwoEnemies = [
  Blinky (Position 1 1) (Position 1 1) East (makeColor (255/255) 0 0 1), 
  Pinky (Position 1 (height - 3)) (Position 1 (height - 3)) West (makeColor (255/255) (177/255) (255/255) 1), 
  Pinky (Position 2 1) (Position 1 (height - 3)) West (makeColor (255/255) (177/255) (255/255) 1), 
  Inky (Position (width - 2) 1) (Position (width - 2) 1) East (makeColor 0 (255/255) (255/255) 1), 
  Clyde (Position (width - 2) (height - 2)) (Position (width - 2) (height - 2)) West (makeColor (255/255) (182/255) (50/255) 1),
  Clyde (Position (width - 3) (height - 2)) (Position (width - 2) (height - 2)) West (makeColor (255/255) (182/255) (50/255) 1)
  ]
  where
    tiles@(first:_) = levelTwoTiles
    width = length first
    height = length tiles

levelTwoTiles :: [[Tile]]
levelTwoTiles = [
  [m, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, b, b, b, x, d, q, b, b, x, d, r],
  [l, d, r, w, w, l, d, r, w, w, w, l, d, r, w, w, l, d, r],
  [l, f, z, t, t, y, d, z, t, t, t, y, d, z, t, t, y, f, r],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, x, d, q, x, d, q, b, b, x, d, r],
  [l, d, z, t, t, y, d, r, l, d, r, l, d, z, t, t, y, d, r],
  [l, d, d, d, d, d, d, r, l, d, r, l, d, d, d, d, d, d, r],
  [p, b, b, b, b, x, d, r, p, b, o, l, d, q, b, b, b, b, o],
  [e, e, e, e, e, l, d, r, m, t, n, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, r, l, d, r, e, e, e, e, e],
  [m, t, t, t, t, y, d, z, y, d, z, y, d, z, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [p, b, b, b, b, x, d, q, x, d, q, x, d, q, b, b, b, b, o],
  [e, e, e, e, e, l, d, r, l, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, r, l, d, r, e, e, e, e, e],
  [e, e, e, e, e, l, d, r, l, d, r, l, d, r, e, e, e, e, e],
  [m, t, t, t, t, y, d, z, y, d, z, y, d, z, t, t, t, t, n],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [l, d, q, b, b, x, d, q, b, b, b, x, d, q, b, b, x, d, r],
  [l, d, z, t, n, l, d, z, t, t, t, y, d, r, m, t, y, d, r],
  [l, f, d, d, r, l, d, d, d, d, d, d, d, r, l, d, d, f, r],
  [p, b, x, d, r, l, d, q, x, d, q, x, d, r, l, d, q, b, o],
  [m, t, y, d, z, y, d, r, l, d, r, l, d, z, y, d, z, t, n],
  [l, d, d, d, d, d, d, r, l, d, r, l, d, d, d, d, d, d, r],
  [l, d, q, b, b, b, b, o, p, b, o, p, b, b, b, b, x, d, r],
  [l, d, z, t, t, t, t, t, t, t, t, t, t, t, t, t, y, d, r],
  [l, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, r],
  [p, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, o]
  ]
  where 
    w = Wall Full
    d = PacDot
    f = PacFruit
    e = Empty
    t = Wall Top
    r = Wall Right
    b = Wall Bottom
    l = Wall Left

    m = Wall CornerFromBottomToRightOutside
    n = Wall CornerFromLeftToBottomOutside
    o = Wall CornerFromTopToLeftOutside
    p = Wall CornerFromRightToTopOutside

    q = Wall CornerFromBottomToRightInside
    x = Wall CornerFromLeftToBottomInside
    y = Wall CornerFromTopToLeftInside
    z = Wall CornerFromRightToTopInside