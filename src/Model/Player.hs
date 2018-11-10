module Model.Player where

import Model.Data

data Player = PacMan { lives :: Int, posPlayer :: Position, dirPlayer :: Direction, intendedDirPlayer  :: Direction }