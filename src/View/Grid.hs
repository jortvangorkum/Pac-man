module View.Grid (viewTiles) where

import Model
import Model.Grid
import Settings
import Helpers
import Graphics.Gloss

import Prelude hiding (Right, Left)
import Data.List
import Data.Foldable (toList)
import Data.Sequence hiding (zip3, replicate, Empty, zip, length, sort, take)

viewTiles :: Grid -> Seq (Tile, Int, Int) -> Picture
viewTiles grid tiles = pictures $ map (tileToPicture grid) (toList tiles)

tileToPicture :: Grid -> (Tile, Int, Int) -> Picture
tileToPicture grid (tile, x, y) = translateToGrid grid x y o
  where
    t = fromIntegral tileSize
    c = case tile of
      (Wall _)  -> color (makeColor (50/255) (35/255) (170/255) 1)
      Empty     -> color black
      PacDot    -> color white
      PacFruit  -> color white
    o = case tile of
      PacDot                                -> c $ circleSolid (t / 8)
      PacFruit                              -> c $ circleSolid (t / 3)
      Empty                                 -> c blank
      (Wall Top)                            -> topWall
      (Wall Right)                          -> rotate 90 topWall
      (Wall Bottom)                         -> rotate 180 topWall
      (Wall Left)                           -> rotate 270 topWall
      (Wall CornerFromBottomToRightOutside) -> bottomRightOutsideWall
      (Wall CornerFromLeftToBottomOutside)  -> rotate 90 bottomRightOutsideWall
      (Wall CornerFromTopToLeftOutside)     -> rotate 180 bottomRightOutsideWall
      (Wall CornerFromRightToTopOutside)    -> rotate 270 bottomRightOutsideWall
      (Wall CornerFromBottomToRightInside)  -> bottomRightInsideWall
      (Wall CornerFromLeftToBottomInside)  -> rotate 90 bottomRightInsideWall
      (Wall CornerFromTopToLeftInside)     -> rotate 180 bottomRightInsideWall
      (Wall CornerFromRightToTopInside)    -> rotate 270 bottomRightInsideWall
      _                                     -> c $ rectangleSolid t t
      where
        topWall = c $ translate 0 (t/4) $ rectangleSolid t (t/2)
        bottomRightOutsideWall = pictures [c $ translate (t/2) (-t/2) $ arcSolid 90 180 t, color black $ translate (t/2) (-t/2) $ arcSolid 90 180 (t/2.2)]
        bottomRightInsideWall = c $ translate (t/2) (-t/2) $ arcSolid 90 180 (t/2.2)