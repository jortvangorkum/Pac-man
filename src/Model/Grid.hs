module Model.Grid where

import Settings
import Prelude hiding (lookup, zip3, Right, Left)
import Data.List hiding (lookup)
import Data.Sequence hiding (length, zip3, replicate, Empty)  

data Grid = Grid { tiles :: Seq (Tile, Int, Int), width :: Int, height :: Int, dots :: Int }
data Tile = Empty | Wall WallType | PacDot | PacFruit deriving (Show, Eq)  
data WallType = Full | Top | Right | Bottom | Left 
              | CornerFromBottomToRightOutside | CornerFromLeftToBottomOutside | CornerFromTopToLeftOutside | CornerFromRightToTopOutside 
              | CornerFromBottomToRightInside | CornerFromLeftToBottomInside | CornerFromTopToLeftInside | CornerFromRightToTopInside 
              | CornerFromBottomToRightOutsideFilled | CornerFromLeftToBottomOutsideFilled | CornerFromTopToLeftOutsideFilled | CornerFromRightToTopOutsideFilled
              deriving (Show, Eq)  

parseGrid :: [[Tile]] -> (Seq (Tile, Int, Int), Int, Int, Int)
parseGrid tiles@(first:_) = (fromList (zip3 (concat tiles) columnIndexArray rowIndexArray), width, height, dots)
  where 
    width = length first 
    height = length tiles
    columnIndexArray = (concat . replicate height) [0 .. width - 1]
    rowIndexArray = concat $ transpose $ replicate width [0 .. height - 1]
    dots = countAmountDots (concat tiles)

countAmountDots :: [Tile] -> Int
countAmountDots = foldr (\tile count -> if tile == PacDot then count + 1 else count) 0