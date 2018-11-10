-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Settings
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Char
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteFile
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- Game Iteration
  | playState gstate == Initialise = do
      highScoreBytes <- ByteFile.readFile "data/highscores.json"
      return $ gstate { 
        playState = Playing, 
        highscores = fromMaybe [] (decode highScoreBytes :: Maybe [Int])
      }

  -- Save highscores
  | playState gstate == SavingHighscore = do
    savedFile <- ByteFile.writeFile "data/highscores.json" (encode (score gstate : highscores gstate))
    return $ gstate {
      playState = Finished,
      highscores = score gstate : highscores gstate
    }
  
  -- finished (via savinghighscores)
  | playState gstate == Finished = return gstate
  | lives (player gstate) <= 0 = return $ gstate { playState = SavingHighscore }
  | dots gstate <= 0 = return $ gstate { playState = SavingHighscore }

  -- paused
  | playState gstate == Paused = return gstate

  -- playing
  | elapsedTime gstate + secs > secondsBetweenCycles && playState gstate == Playing = do 
      -- Random Directions
      rdirs <- mapM (`randomDirection` grid gstate) (nextEnemies gstate)
      -- Chase Player Directions
      -- rdirs <- return $ map (\enemy -> pathFinding enemy ((posPlayer . player) gstate) (grid gstate)) (nextEnemies gstate) 
      return $ gstate {
        -- grid
        grid = gridAfterUpdate (grid gstate) (nextPlayer gstate),
        -- movables
        player = interactPlayerWithEnemies (nextPlayer gstate) (updatePlayer (nextPlayer gstate) (grid gstate)) (nextEnemies gstate), 
        nextPlayer = updatePlayer (interactPlayerWithEnemies (nextPlayer gstate) (updatePlayer (nextPlayer gstate) (grid gstate)) (nextEnemies gstate)) (grid gstate), 
        enemies = nextEnemies gstate,
        nextEnemies = updateEnemies (zip (nextEnemies gstate) rdirs),
        -- time
        elapsedTime = 0,
        cyclesPassed = cyclesPassed gstate + 1,
        score = updateScore (score gstate) (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)),
        dots = updateAmountDots (dots gstate) (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)),
        ghostMode = updateGhostMode (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)) (ghostMode gstate) (invincibilityBegin gstate) (cyclesPassed gstate),
        invincibilityBegin = updateInvicibilityBegin (getTileFromGrid (grid gstate) ((posPlayer . nextPlayer) gstate)) (invincibilityBegin gstate) (cyclesPassed gstate)
      }
  -- Just update the elapsed time
  | otherwise = 
    return $ gstate { 
      elapsedTime = elapsedTime gstate + secs
    }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = case c of
  '1' -> gstate { grid = levelOneGrid }
  '2' -> gstate { grid = levelTwoGrid }
  'p' -> gstate { playState = togglePause (playState gstate) }
  'r' -> initialState
  'w' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  's' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  'a' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  'd' -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }
  _   -> gstate 

inputKey (EventKey (SpecialKey s) Down _ _) gstate = case s of
  KeyUp    -> gstate { nextPlayer = tryDirect (nextPlayer gstate) North (grid gstate) }
  KeyDown  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) South (grid gstate) }
  KeyLeft  -> gstate { nextPlayer = tryDirect (nextPlayer gstate) West (grid gstate) }
  KeyRight -> gstate { nextPlayer = tryDirect (nextPlayer gstate) East (grid gstate) }
  _        -> gstate

inputKey _ gstate = gstate

direct :: Player -> Direction -> Player
direct player direction' = player { dirPlayer = direction' }

tryDirect :: Player -> Direction -> Grid -> Player
tryDirect player direction' grid = player { intendedDirPlayer  = direction' }

movePlayer :: Player -> Direction -> Player
movePlayer player@PacMan{posPlayer = pos} dir = player { posPlayer = move pos dir} 

move :: Position -> Direction -> Position
move (Position x y) North = Position x (y-1)
move (Position x y) East = Position (x + 1) y
move (Position x y) South = Position x (y+1)
move (Position x y) West = Position (x - 1) y

togglePause :: PlayState -> PlayState
togglePause Playing = Paused
togglePause Paused = Playing
togglePause Finished = Finished

-- also passes nextEnemies since otherwise the player and enemy can cross eachother without knowing
interactPlayerWithEnemies :: Player -> Player -> [Enemy] -> Player
interactPlayerWithEnemies player nextPlayer enemies = updatePosition p'
  where 
    p'= player { lives = foldr (checkPlayerAndEnemyPosition nextPlayer) (lives p) enemies }
    p = player { lives = foldr (checkPlayerAndEnemyPosition player) (lives player) enemies }

    updatePosition :: Player -> Player
    updatePosition updatedPlayer
      | lives player /= lives updatedPlayer = updatedPlayer { posPlayer = initialPlayerPosition, dirPlayer = East }
      | otherwise                           = updatedPlayer
      
    -- check if the position is the same, since in that case you are sure they are collided
    checkPlayerAndEnemyPosition :: Player -> Enemy -> Int -> Int
    checkPlayerAndEnemyPosition player enemy lives'
      | lives player /= lives'             = lives'
      | posEnemy enemy == posPlayer player = lives' - 1
      | otherwise                          = lives'

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

gridAfterUpdate :: Grid -> Player -> Grid
gridAfterUpdate grid updatePlayer = case tile of
  PacDot   -> updateTileOfGrid grid positionAfterUpdate Empty
  PacFruit -> updateTileOfGrid grid positionAfterUpdate Empty
  _        -> grid
  where
    positionAfterUpdate = posPlayer updatePlayer
    tile = getTileFromGrid grid positionAfterUpdate

{-
  Ghosts
-}
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

randomDirection :: Enemy -> Grid -> IO Direction
randomDirection enemy grid = case dirs of
    [] -> return (oppositeDirection (dirEnemy enemy))
    _  -> pickElement dirs
    where dirs = filter (\dir -> dir /= oppositeDirection (dirEnemy enemy)) (getDirections grid (posEnemy enemy))

updateEnemies :: [(Enemy, Direction)]-> [Enemy]
updateEnemies enemies = map updateEnemy enemies

updateEnemy :: (Enemy, Direction) -> Enemy
updateEnemy (enemy, rdir) = enemy { posEnemy = move (posEnemy enemy) rdir, dirEnemy = rdir }

pickElement :: [a] -> IO a
pickElement [] = error "pickElement: expects a non-empty list"
pickElement [x] = return x
pickElement list = do
    i <- randomRIO (0, length list - 1)
    return $ list !! i

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East

updateScore :: Int -> Tile -> Int
updateScore score tile = case tile of
  PacDot   -> score + 10
  PacFruit -> score + 50
  _        -> score

updateAmountDots :: Int -> Tile -> Int
updateAmountDots dots tile = case tile of
  PacDot -> dots - 1
  _      -> dots

updateGhostMode :: Tile -> GhostMode -> Int -> Int -> GhostMode
updateGhostMode PacFruit _ _ _ = Frightened
updateGhostMode _ ghostMode begin current
  | check     = Chase
  | otherwise = ghostMode
  where
    check = current - begin >= invincibilityCycles && begin > 0

updateInvicibilityBegin :: Tile -> Int -> Int -> Int
updateInvicibilityBegin PacFruit _ current = current
updateInvicibilityBegin _ begin _ = begin

pathFinding :: Enemy -> Position -> Grid -> Direction
pathFinding enemy target grid
  | length possibleDirections > 1 = getBestDirection possibleDirections current target
  | otherwise                     = dir
  where
    possibleDirections :: [Direction]
    possibleDirections = filter (\dir -> dir /= oppositeDirection (dirEnemy enemy)) (getDirections grid current)
    (dir:_) =  possibleDirections
    current = posEnemy enemy

getBestDirection :: [Direction] -> Position -> Position -> Direction
getBestDirection dirs@(dir:_) current target
  | abs (y2 - y1) > abs (x2 - x1) = checkUpDown True
  | otherwise                     = checkLeftRight True
  where
    x1 = x current
    x2 = x target
    y1 = y current
    y2 = y target
    checkDirection :: [Direction] -> Direction -> Maybe Direction
    checkDirection dirs dir
      | dir `elem` dirs = Just dir
      | otherwise     = Nothing
    checkLeftRight :: Bool -> Direction
    checkLeftRight checkOtherDirection = if x2 - x1 < 0
      then case checkDirection dirs West of 
        Just x -> x
        _ -> if checkOtherDirection then checkUpDown False else dir
      else case checkDirection dirs East of 
        Just x -> x
        _ -> if checkOtherDirection then checkUpDown False else dir
    checkUpDown :: Bool -> Direction
    checkUpDown checkOtherDirection = if y2 - y1 < 0
      then case checkDirection dirs North of
        Just x -> x
        _ -> if checkOtherDirection then checkLeftRight False else dir
      else case checkDirection dirs South of
        Just x -> x
        _ -> if checkOtherDirection then checkLeftRight False else dir


