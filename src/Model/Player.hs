module Model.Player where

import Model.Data

data Player = PacMan { lives :: Int, posPlayer :: Position, initPosPlayer :: Position, dirPlayer :: Direction, intendedDirPlayer  :: Direction }