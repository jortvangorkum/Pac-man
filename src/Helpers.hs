module Helpers where

import Model
import Model.Grid
import Settings
import Graphics.Gloss
import Data.Maybe
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (zip3, replicate, Empty)



{-
  View helpers
-}

-- first translate from center to top left, then translate from grid index to screen position, then offset by tile size from center to top left
translateToGrid :: Grid -> Int -> Int -> (Picture -> Picture)
translateToGrid grid column row = translate 0 (-(fromIntegral topScoreBarSize/2)) . translate width' height' . translate column' row' . translate (tileSize' / 2) (-tileSize' / 2)
  where
    width' = -(fromIntegral (width grid * tileSize) / 2)
    height' = fromIntegral(height grid * tileSize) / 2
    column' = fromIntegral column * tileSize'
    row' = -(fromIntegral row * tileSize')
    tileSize' = fromIntegral tileSize

extraTranslation :: Int -> Int -> Float -> (Picture -> Picture)
extraTranslation dx dy time = translate dx' dy'
  where
    dx' = fromIntegral dx * extraTranslationAmount
    dy' = fromIntegral dy * extraTranslationAmount
    extraTranslationAmount = (time / secondsBetweenCycles) * fromIntegral tileSize

sizeFromPercentage :: Float -> Float
sizeFromPercentage size = fromIntegral tileSize * size

viewTopBar :: Grid -> Picture -> Picture
viewTopBar grid picture = translate 0 (fromIntegral spaceForSides / 1.25) $ translateToGrid grid 0 0 picture

viewText :: Float -> Picture -> Picture
viewText size picture = scale ((t / 30) * size) ((t / 30) * size) $ color white picture
  where
    t = fromIntegral tileSize

{-
  Controller helpers
-}

move :: Position -> Direction -> Position
move (Position x y) North = Position x (y-1)
move (Position x y) East = Position (x + 1) y
move (Position x y) South = Position x (y+1)
move (Position x y) West = Position (x - 1) y

getDirections :: Grid -> Position -> [Direction]
getDirections grid (Position x y) = mapMaybe tileToDirection [(north, North), (east, East), (south, South), (west, West)]
  where
    north = getTileFromGrid grid (Position x (y-1))
    east = getTileFromGrid grid (Position (x+1) y) 
    south = getTileFromGrid grid (Position x (y+1))
    west = getTileFromGrid grid (Position (x-1) y) 
    tileToDirection :: (Tile, Direction) -> Maybe Direction
    tileToDirection (tile, dir) = case tile of 
      (Wall _) -> Nothing
      _        -> Just dir

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East


{-
  Model helpers
-}

indexFromPosition :: Grid -> Position -> Int
indexFromPosition grid (Position x y) = x + width grid * y

halfNegativeWindowSizeFromGrid :: Grid -> (Float, Float)
halfNegativeWindowSizeFromGrid (Grid _ w h _) = (-(fromIntegral w * 15), fromIntegral h * 15) 

windowSizeFromGrid :: Grid -> (Int, Int)
windowSizeFromGrid (Grid _ w h _) = (w * tileSize, h * tileSize) 

getTileFromGrid :: Grid -> Position -> Tile
getTileFromGrid grid@(Grid tiles _ _ _) position = case lookup (indexFromPosition grid position) tiles of
  Just (tile, _, _) -> tile
  _                 -> error "Position is outside of the Grid"

getTileFromTuple :: (Tile, Int, Int) -> Tile
getTileFromTuple (tile, _, _) = tile

getNextTileFromPlayer :: Player -> Grid -> Tile
getNextTileFromPlayer player grid = getTileFromGrid grid (getNextPositionFromPlayer player)

getNextPositionFromPlayer :: Player -> Position
getNextPositionFromPlayer player@PacMan{dirPlayer = North, posPlayer = (Position x y)} = Position x (y-1)
getNextPositionFromPlayer player@PacMan{dirPlayer = East, posPlayer = (Position x y)}  = Position (x+1) y
getNextPositionFromPlayer player@PacMan{dirPlayer = South, posPlayer = (Position x y)} = Position x (y+1)
getNextPositionFromPlayer player@PacMan{dirPlayer = West, posPlayer = (Position x y)}  = Position (x-1) y

updateTileOfGrid :: Grid -> Position -> Tile -> Grid
updateTileOfGrid grid position@(Position x y) tile = grid { tiles = update (indexFromPosition grid position) (tile, x, y) (tiles grid) }