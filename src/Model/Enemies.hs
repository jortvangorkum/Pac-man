module Model.Enemies where

import Model.Data
import Graphics.Gloss

data Enemy = 
  Blinky { posEnemy :: Position, initPosEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Pinky { posEnemy :: Position, initPosEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Inky { posEnemy :: Position, initPosEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 
  | Clyde { posEnemy :: Position, initPosEnemy :: Position, dirEnemy :: Direction, colEnemy :: Color } 


data GhostMode = Chase | Scatter | Frightened