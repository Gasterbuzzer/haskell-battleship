import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Network.MateLight
import Network.MateLight.Simple
import qualified Network.Socket as Sock
import SDLEventProvider.SDLKeyEventProvider

-- New Imports
import Data.List

--                status  key     time
type KeyStatus = (String, String, Integer)

type MyState = (Int, Int, Int, Int, Int, [[Int]], [[Int]], [[Int]], [[Int]], Int, Int)
-- (x, y,.. which represent cursors position
-- Color Cursor
-- Cursor Mode
-- Gameplay Mode (0-8) (0 == Main Menu/Nothing, 1 == Player One Place Ships, 2 == Player Two Place Ships, 3-4 == Player One Attack, 5-6 == Player Two Attack, 7 == Player One Won, 8 == Player Two won)
-- List containing all ship attack positions.
-- List containing enemy ship attack positions
-- List containing all placed ships for player one
-- List containing all placed ships for player two

initState :: MyState -- initial state
initState = (3, 1, 2, 2, 3, [[]], [[]], [[]], [[]], 0, 0) -- For Info see type above.

-- 0 for x dimension (waagerecht)
-- 1 for y dimension (senkrecht)
type Rotation = Int
type ShipLength = Int

dim :: (Int, Int) -- wall dimensions
dim = (30, 12)

movedim :: (Int, Int)
movedim = (10, 10)

----------------------------------------------------------------------------------
-- Key Functions.
----------------------------------------------------------------------------------
getKeyDataTuples keyState =
  map (\(k, t) -> ("Pressed", k, t)) (pressed keyState)
    ++ map (\(k, d) -> ("Held", k, d)) (held keyState)
    ++ map (\(k, t) -> ("Released", k, t)) (released keyState)

getButtonDataTuples buttonState =
  map (\(k, t) -> ("Pressed", k, t)) (pressedB buttonState)
    ++ map (\(k, d) -> ("Held", k, d)) (heldB buttonState)
    ++ map (\(k, t) -> ("Released", k, t)) (releasedB buttonState)


----------------------------------------------------------------------------------
-- Key Update States.
----------------------------------------------------------------------------------

--      wall dimens   input event  state      new state
move :: (Int, Int) -> KeyStatus -> MyState -> MyState
-- MOVE MODE 2 ROTATION 0
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (x, (y `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
-- MOVE MODE 2 ROTATION 1
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (x, (y `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 2, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
-- MOVE MODE 3 ROTATION 0
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (x, (y `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) = if dur >= 100 then (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 0, cship)
-- MOVE MODE 3 ROTATION 1
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (x, (y `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) = if dur >= 100 then (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship) else (x, y, 2, 3, c, attacks, attacks2, playerShips1, playerShips2, 1, cship)
-- MOVE MODE 0 ROTATION _
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (if x > 10 || x < 4 then ((x - 1) `mod` xdim) + 10 else (x - 1) `mod` xdim, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (x, (y `mod` ydim) + 1, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (if x == 10 || x == 11 then (x `mod` xdim) + 11 else (x `mod` xdim) + 1, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 0, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
-- MOVE MODE 1 ROTATION _
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (x, ((y - 2) `mod` ydim) + 1, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (if x > 20 || x < 18 then ((x - 1) `mod` xdim) + 20 else ((x - 1) `mod` xdim) + 10, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (x, (y `mod` ydim) + 1, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (x, (y `mod` ydim) + 1, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = if dur >= 100 then (if x < 19 || x > 25 then ((x + 1) `mod` xdim) + 10 else ((x + 1) `mod` xdim) + 20, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) else (x, y, 2, 1, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
-- MOVE RETURN MODE 2 CSHIP 4
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 2, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, 1) = (17, 1, 2, 3, currentLevel, attacks, attacks2, playerShips1,
                                                                                                     drop 1 playerShips2  ++ newShip (constShips !! 4) crotation (x, y, cursorColor, 3, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, 4),
                                                                                                     crotation, 0)
-- MOVE RETURN MODE 2 CSHIP _
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 2, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (3, 1, 2, 2, currentLevel, attacks, attacks2, playerShips1,
                                                                                                     playerShips2 ++ newShip (constShips !! cship) crotation (x, y, cursorColor, 3, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship),
                                                                                                     crotation, cship + 1)
-- MOVE RETURN MODE 3 CSHIP 4
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 3, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, 1) = (3, 1, 2, 0, currentLevel, attacks, attacks2,
                                                                                                     drop 1 playerShips1 ++ newShip (constShips !! 4) crotation (x, y, cursorColor, 2, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, 4),
                                                                                                     playerShips2, crotation, 0)
-- MOVE RETURN MODE 3 CSHIP _
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 3, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (17, 1, 2, 3, currentLevel, attacks, attacks2,
                                                                                                     playerShips1 ++ newShip (constShips !! cship) crotation (x, y, cursorColor, 2, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship),
                                                                                                     playerShips2, crotation, cship + 1)
                                                                                                     
-- MOVE RETURN MODE 0
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 0, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if check (x, y,cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips2) == 1 then x else 17,
                                                                                                     if check (x, y,cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips2) == 1 then y else 1,
                                                                                                     check (x, y, cursorColor, 0, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips2),
                                                                                                     if check (x, y,cursorColor, 0, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips2) == 1 then 0 else 1,
                                                                                                     (if ((checkIfTwoPlayerFinished (attacks, attacks2, playerShips1, playerShips2)) /= 0) then (4) else (1)),
                                                                                                     (addShipAttack1 (x, y, cursorColor, 0, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship)),
                                                                                                     (addShipAttack2 (x, y, cursorColor, 0, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship)),
                                                                                                     playerShips1,
                                                                                                     playerShips2,
                                                                                                     crotation,
                                                                                                     cship)
-- MOVE RETURN MODE 1
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (if check (x, y,cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips1) == 1 then x else 3,
                                                                                                     if check (x, y,cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips1) == 1 then y else 1,
                                                                                                     check (x, y, cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips1),
                                                                                                     if check (x, y,cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) (playerShips1) == 1 then 1 else 0,
                                                                                                     (if ((checkIfTwoPlayerFinished (attacks, attacks2, playerShips1, playerShips2)) /= 0) then (4) else (1)),
                                                                                                     (addShipAttack1 (x, y, cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship)),
                                                                                                     (addShipAttack2 (x, y, cursorColor, 1, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship)),
                                                                                                     playerShips1,
                                                                                                     playerShips2,
                                                                                                     crotation,
                                                                                                     cship)
-- MOVE SPACE ROTATION 0
move (xdim, ydim) ("Pressed", "SPACE", _) (x, y, cursorColor, cursorMode, currentLevel, attacks, attacks2, playerShips1, playerShips2, 0, cship) = (x, y, cursorColor, cursorMode, currentLevel, attacks, attacks2, playerShips1, playerShips2, 1, cship)
-- MOVE SPACE ROTATION 1
move (xdim, ydim) ("Pressed", "SPACE", _) (x, y, cursorColor, cursorMode, currentLevel, attacks, attacks2, playerShips1, playerShips2, 1, cship) = (x, y, cursorColor, cursorMode, currentLevel, attacks, attacks2, playerShips1, playerShips2, 0, cship)

move _ _ (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship)

constShips = [4, 4, 3, 3, 2]

check :: MyState -> [[Int]] -> Int
check (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) xxs | length (helper (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) xxs) == 1      = 1
                                                                                             | otherwise                                                          = 4
    where helper (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) xxs = [1 | xs <- xxs, head xs == x' && xs !! 1 == y']

checkAttack1 :: MyState -> [[Int]] -> Int
checkAttack1 (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) level | length (helper (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) level) == 1 = 1
                                                                                                      | otherwise                                                       = 4
    where helper (x', y', a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) level                                                               = [1 | xs <- (level), head xs == x' && xs !! 1 == y']

addShipAttack1 :: MyState -> [[Int]]
addShipAttack1 (x', y', a, 0, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [[x', y', checkAttack1 (x', y', a, 0, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) playerShips2]] ++ attacks
addShipAttack1 (x', y', a, 1, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [] ++ attacks

addShipAttack2 :: MyState -> [[Int]]
addShipAttack2 (x', y', a, 0, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [] ++ attacks2
addShipAttack2 (x', y', a, 1, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [[x', y', checkAttack1 (x', y', a, 1, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) playerShips1]] ++ attacks2

addShip :: MyState -> [[Int]]
addShip (x', y', a, b, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [[x', y', check (x', y', a, b, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) [[1,2,3]]]] ++ attacks

----------------------------------------------------------------------------------
-- Frame Functions
----------------------------------------------------------------------------------

toFrameList :: (Int, Int) -> [[Int]] -> MyState -> ListFrame
toFrameList (xdim, ydim) pixels (xC, yC, cursorColor, cursorMode, level, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = case level of
  0 -> ListFrame $ [ ([ if (any (==True) [True | [x',y', infoPixel] <- ([[xC, yC, cursorColor]]++pixels), x' == x, y' == y]) then (pixelType (getInfoPixel ([[xC, yC, cursorColor]]++pixels) [x, y])) else (pixelType 0)  | x <- [0 .. xdim - 1]]) | y <- [0 .. ydim - 1]]
      where
        pixelType info = case info of
          0 -> (Pixel 115 241 255) -- Blue Sea
          1 -> (Pixel 255 0 0) -- Red
          2 -> (Pixel 120 120 120) -- Grey
          3 -> (Pixel 255 132 0) -- Orange
          4 -> (Pixel 21 0 255) -- Dark Blue
          5 -> (Pixel 145 71 54) -- Brown
          6 -> (Pixel 234 255 0) -- Yellow
  1 -> toFrameList (xdim, ydim) ((convertLevelPixel (xdim, ydim) [attacks2])++(convertLevelPixel (xdim, ydim) [attacks])++levelHudBorders++(convertLevelPixel (xdim, ydim) [attacks2])) (xC, yC, cursorColor, cursorMode, 0, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
  2 -> toFrameList (xdim, ydim) ((convertLevelPixel (xdim, ydim) [attacks2])++(convertLevelPixel (xdim, ydim) [attacks])++levelHudBorders++(convertLevelPixel (xdim, ydim) [attacks2])) (xC, yC, cursorColor, cursorMode, 0, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
  3 -> toFrameList (xdim, ydim) ((if cursorMode == 3 then (convertLevelPixel (xdim, ydim) [playerShips2]) else [])++(if cursorMode == 2 then (convertLevelPixel (xdim, ydim) [playerShips1]) else [])++levelHudBorders) (xC, yC, cursorColor, cursorMode, (if (cursorMode == 0 || cursorMode == 1) then 1 else 0), attacks, attacks2, playerShips1, playerShips2, crotation, cship)
  4 -> toFrameList (xdim, ydim) (endScreenPixel) (xC, yC, cursorColor, cursorMode, 0, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
  9 -> toFrameList (xdim, ydim) ((levelShipHud 1 [[2,3],[18,3],[18,4],[18,5],[18,6],[18,2], [18,2]])) (xC, yC, cursorColor, cursorMode, 0, attacks, attacks2, playerShips1, playerShips2, crotation, cship)
  _ -> toFrameList (xdim, ydim) ((convertLevelPixel (xdim, ydim) [attacks2])++(convertLevelPixel (xdim, ydim) [attacks])++levelHudBorders++(convertLevelPixel (xdim, ydim) [attacks2])) (xC, yC, cursorColor, cursorMode, 0, attacks, attacks2, playerShips1, playerShips2, crotation, cship)

getInfoPixel :: [[Int]] -> [Int] -> Int
getInfoPixel pixels [x, y] = [infoPixel | [x', y', infoPixel] <- pixels, x' == x, y' == y] !! 0

----------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------

--           input events      state      frame       new state
eventMain :: [Event String] -> MyState -> (ListFrame, MyState)
eventMain events state = trace (show state) (toFrameList dim helloTextPixel state', state')
  where
    state' =
      foldl
        ( \acc (Event mod ev) -> case mod of
            "SDL_KEY_DATA" ->
              foldl (flip (move movedim)) acc (getKeyDataTuples (read ev :: KeyState))
            "SDL_JOYSTICK_DATA" ->
              foldl (flip (move movedim)) acc (getButtonDataTuples (read ev :: ButtonState))
            _ ->
              acc
        )
        state
        events


----------------------------------------------------------------------------------
-- Level Functions/Variables
----------------------------------------------------------------------------------

-- BEGIN BATTLESHIP
one :: [[[Int]]]
one = [[[11,3],[11,4],[11,5],[11,6]],
       [[4,1],[4,2],[4,3],[4,4]],
       [[6,4],[7,4],[8,4]],
       [[3,9],[4,9],[5,9]],
       [[11,10],[12,10]]]

twop1 :: [[[Int]]]
twop1 = [[[9,3],[10,3],[11,3],[12,3]],
       [[9,7],[9,8],[9,9],[9,10]],
       [[3,3],[4,3],[5,3]],
       [[4,8],[5,8],[6,8]],
       [[6,1],[7,1]]]

threep1 :: [[[Int]]]
threep1 = [[[7,6],[8,6],[9,6],[10,6]],
         [[4,6],[4,7],[4,8],[4,9]],
         [[4,1],[5,1],[6,1]],
         [[12,8],[12,9],[12,10]],
         [[8,1],[8,2]]]

fourthp1 :: [[[Int]]]
fourthp1 = [[[0,0]]]


onep2 :: [[[Int]]]
onep2 = [[[25,3],[25,4],[25,5],[25,6]],
       [[18,1],[18,2],[18,3],[18,4]],
       [[20,4],[21,4],[22,4]],
       [[17,9],[18,9],[19,9]],
       [[25,10],[26,10]]]

two :: [[[Int]]]
two = [[[23,3],[24,3],[25,3],[26,3]],
       [[23,7],[23,8],[23,9],[23,10]],
       [[17,3],[18,3],[19,3]],
       [[18,8],[19,8],[20,8]],
       [[20,1],[21,1]]]

threep2 :: [[[Int]]]
threep2 = [[[21,6],[22,6],[23,6],[24,6]],
         [[18,6],[18,7],[18,8],[18,9]],
         [[18,1],[19,1],[20,1]],
         [[26,8],[26,9],[26,10]],
         [[22,1],[22,2]]]

three :: [[[Int]]]
three = [[[0,0]]]

fourth :: [[[Int]]]
fourth = [[[0,0]]]

levelHudBorders :: [[Int]]
levelHudBorders = [[2, y, 5] | y <- [0..10]] ++ [[13, y, 5] | y <- [0..10]] ++ [[x, 0, 5] | x <- [2..13]] ++ [[x, 11, 5] | x <- [2..13]]           ++ [[16, y, 5] | y <- [0..10]] ++ [[27, y, 5] | y <- [0..10]] ++ [[x, 0, 5] | x <- [16..27]] ++ [[x, 11, 5] | x <- [16..27]]
-- levelHudBorders: First part left Side, Second part Right Side, Third Part Top, Fourth Part Bottom

levelShipHud :: Int -> [[Int]] -> [[Int]]
levelShipHud levelNumber attack = [ [(1 + (currentShipSize)), (1 + (shipNumber * 2)), 2] | (shipSize, shipNumber) <- (zip (menuShipControl attack (convertNumberLevel levelNumber)) [0..]), currentShipSize <- [1..shipSize]]           ++           [ [(1 + (currentShipSize)), 1+( (length((menuShipControl attack (convertNumberLevel levelNumber)))*2) + (shipNumber * 2)), 1] | (shipSize, shipNumber) <- (zip (removeItemList (menuShipControl attack (convertNumberLevel levelNumber)) (shipsInLevel levelNumber) (length (menuShipControl attack (convertNumberLevel levelNumber)))) [0..]), currentShipSize <- [1..shipSize]]

newShip :: ShipLength -> Rotation -> MyState -> [[Int]]
newShip 1 _ (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = [[x,y]]
newShip n 0 (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) | (x + n - 1) < 27 = newShip (n-1) 0 (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) ++ [[x + n - 1, y]]
                                               | otherwise = error "ship out of map"
newShip n 1 (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) | (y + n - 1) < 11 = newShip (n-1) 1 (x, y, a, b, c, attacks, attacks2, playerShips1, playerShips2, crotation, cship) ++ [[x, y + n - 1]]
                                                | otherwise = error "ship out of map"
newShip _ _ _ = error "mhh something went wrong"

rotation :: Rotation -> Rotation
rotation 0 = 1
rotation 1 = 0
rotation _ = error "mhh something went wrong"

checkIfLevelFinished :: [[[Int]]] -> MyState -> Bool
checkIfLevelFinished level (x, y, cursorColor, cursorMode, currentLevel, attacks, attacks2, playerShips1, playerShips2, crotation, cship) = (length(convertLevelPixelLevel dim level)) == (length([ True | [xS, yS, colorS] <- (attacks), elem ([xS, yS]) (convertLevelPixelLevel dim level)]))

checkIfTwoPlayerFinished :: ([[Int]], [[Int]], [[Int]], [[Int]]) -> Int
checkIfTwoPlayerFinished (attacks, attacks2, playerShips1, playerShips2) | (length(playerShips2)) == (length([ True | [xS, yS, colorS] <- (nub attacks), elem ([xS, yS]) (playerShips2)])) = 1
                                                                         | (length(playerShips1)) == (length([ True | [xS, yS, colorS] <- (nub attacks2), elem ([xS, yS]) (playerShips1)])) = 2
                                                                         | otherwise = 0

----------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------

convertLevelPixel :: (Int, Int) -> [[[Int]]] -> [[Int]]
convertLevelPixel dim level = [ [xS, yS, color] | ship <- level, [xS, yS, color] <- ship]

convertLevelPixelLevel :: (Int, Int) -> [[[Int]]] -> [[Int]]
convertLevelPixelLevel dim level = [ [xS, yS] | ship <- level, [xS, yS] <- ship]

convertLevelPixelLevelTest :: (Int, Int) -> [[[Int]]] -> [[Int]]
convertLevelPixelLevelTest dim level = [ [xS, yS, 1] | ship <- level, [xS, yS] <- ship]

convertNumberLevel :: Int -> [[[Int]]]
convertNumberLevel number | number == 1 = one
                          | number == 2 = two
                          | number == 3 = three
                          | number == 4 = fourth
                          | otherwise = one

emptyAttack :: [[Int]]
emptyAttack = [[]]

increaseCurrentLevel :: Int -> Bool -> Int
increaseCurrentLevel levelNumber finishedBool | finishedBool = (levelNumber + 1)
                                              | otherwise = levelNumber

shouldIEmpty :: [[Int]] -> Bool -> [[Int]]
shouldIEmpty attacks finishedBool | finishedBool = emptyAttack
                                  | otherwise = attacks

shipsInLevel :: Int -> [Int]
shipsInLevel levelNumber = sort [ length(ship) | ship <- (convertNumberLevel(levelNumber))]

menuShipControl :: [[Int]] -> [[[Int]]] -> [Int]
menuShipControl xxs yys = controlEach (nub xxs) yys
    where controlEach xxs [] = []
          controlEach xxs (y:ys) | [ys | ys <- y, [x'',y'',color] <- xxs, ys == [x'',y'']] == y = controlEach xxs ys
                                 | otherwise = length y : controlEach xxs ys

removeItem :: Int -> [Int] -> [Int]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys

removeItemList :: [Int] -> [Int] -> Int -> [Int]
removeItemList elements listRemove elementAmount | elementAmount <= 0 = listRemove
                                                 | otherwise          = removeItemList (tail elements) (removeItem (head elements) listRemove) (elementAmount-1)
----------------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------------

ip :: String -- IP address of wall/emulator
ip = "134.28.70.51" -- "127.0.0.1"

delay :: Int -- delay between frames in microseconds
delay = 33000

----------------------------------------------------------------------------------
-- Testing Functions/Variables
----------------------------------------------------------------------------------

-- Text Hello as Pixels
helloTextPixel = [[0, 0, 2], [0, 1, 2], [0, 2, 2], [1, 1, 2], [2, 0, 2], [2, 1, 2], [2, 2, 2], [4, 0, 1], [4, 1, 1], [4, 2, 1], [4, 3, 1], [4, 4, 1], [5, 0, 1], [5, 2, 1], [5, 4, 1], [7, 0, 3], [7, 1, 3], [7, 2, 3], [8, 2, 3], [10, 0, 3], [10, 1, 3], [10, 2, 3], [11, 2, 3], [13, 0, 4], [14, 0, 4], [15, 0, 4], [13, 2, 4], [14, 2, 4], [15, 2, 4], [13, 1, 4], [15, 1, 4]]

-- End Screen
endScreenPixel = [[0, 0, 1], [1, 1, 1], [1, 2, 1], [2, 0, 1], [4, 0, 2], [4, 1, 2], [4, 2, 2], [5, 2, 2], [5, 0, 2], [6, 0, 2], [6, 1, 2], [6, 2, 2], [8, 0, 3], [8, 1, 3], [9, 2, 3], [10, 0, 3], [10, 1, 3], [14, 0, 4], [15, 1, 4], [16, 0, 4], [17, 1, 4], [18, 0, 4], [20, 0, 5], [20, 1, 5], [20, 2, 5], [22, 0, 6], [22, 1, 6], [22, 2, 6], [22, 3, 6], [23, 1, 6], [24, 2, 6], [25, 0, 6], [25, 1, 6], [25, 2, 6], [25, 3, 6]]

----------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------

main :: IO ()
main = do
  window <- showSDLControlWindow
  Sock.withSocketsDo $
    runMate
      (Config (fromJust $ parseAddress ip) 1337 dim (Just delay) True [sdlKeyEventProvider, sdlJoystickEventProvider])
      eventMain
      initState
  destroySDLControlWindow window
