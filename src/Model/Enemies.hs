module Model.Enemies where

import Model.Data
import Graphics.Gloss

data Enemy = 
  Blinky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Pinky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Inky { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Clyde { posEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 


data GhostMode = Chase | Scatter | Frightened