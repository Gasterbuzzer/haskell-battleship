import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Network.MateLight
import Network.MateLight.Simple
import qualified Network.Socket as Sock
import SDLEventProvider.SDLKeyEventProvider

--                status  key     time
type KeyStatus = (String, String, Integer)

dim :: (Int, Int) -- wall dimensions
dim = (30, 12)

movedim :: (Int, Int)
movedim = (10, 10)

getKeyDataTuples keyState =
  map (\(k, t) -> ("Pressed", k, t)) (pressed keyState)
    ++ map (\(k, d) -> ("Held", k, d)) (held keyState)
    ++ map (\(k, t) -> ("Released", k, t)) (released keyState)

getButtonDataTuples buttonState =
  map (\(k, t) -> ("Pressed", k, t)) (pressedB buttonState)
    ++ map (\(k, d) -> ("Held", k, d)) (heldB buttonState)
    ++ map (\(k, t) -> ("Released", k, t)) (releasedB buttonState)

-- BEGIN MAIN FUNCTIONALITY

type MyState = (Int, Int, Int, Int, Int)

--      wall dimens   input event  state      new state
move :: (Int, Int) -> KeyStatus -> MyState -> MyState
move (xdim, ydim) ("Pressed", "P0_Axis_1_D0", _) (x, y, a, b, c) = (x, ((y - 2) `mod` ydim) + 1, a, b, c)
move (xdim, ydim) ("Held", "P0_Axis_1_D0", dur) (x, y, a, b, c) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "P0_Axis_0_D0", _) (x, y, a, b, c) = (((x - 1) `mod` xdim) + 10, y, a, b, c)
move (xdim, ydim) ("Held", "P0_Axis_0_D0", dur) (x, y, a, b, c) = if dur >= 100 then (((x - 1) `mod` xdim) + 10, y, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "P0_Axis_1_D1", _) (x, y, a, b, c) = (x, (y `mod` ydim) + 1, a, b, c)
move (xdim, ydim) ("Held", "P0_Axis_1_D1", dur) (x, y, a, b, c) = if dur >= 100 then (x, (y `mod` ydim) + 1, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "P0_Axis_0_D1", _) (x, y, a, b, c) = (((x + 1) `mod` xdim) + 10, y, a, b, c)
move (xdim, ydim) ("Held", "P0_Axis_0_D1", dur) (x, y, a, b, c) = if dur >= 100 then (((x + 1) `mod` xdim) + 10, y, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "UP", _) (x, y, a, b, c) = (x, ((y - 2) `mod` ydim) + 1, a, b, c)
move (xdim, ydim) ("Held", "UP", dur) (x, y, a, b, c) = if dur >= 100 then (x, ((y - 2) `mod` ydim) + 1, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y, a, b, c) = (((x - 1) `mod` xdim) + 10, y, a, b, c)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y, a, b, c) = if dur >= 100 then (((x - 1) `mod` xdim) + 10, y, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y, a, b, c) = (x, (y `mod` ydim) + 1, a, b, c)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y, a, b, c) = if dur >= 100 then (x, (y `mod` ydim) + 1, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y, a, b, c) = (((x + 1) `mod` xdim) + 10, y, a, b, c)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y, a, b, c) = if dur >= 100 then (((x + 1) `mod` xdim) + 10, y, a, b, c) else (x, y, a, b, c)
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, 1, b, c) = (x, y, 0, b, c)
move (xdim, ydim) ("Pressed", "RETURN", _) (x, y, 0, b, c) = (x, y, 1, b, c)
move (xdim, ydim) ("Pressed", "SPACE", _) (x, y, a, 0, c) = (x, y, a, 1, c)
move (xdim, ydim) ("Pressed", "SPACE", _) (x, y, a, 1, c) = (x, y, a, 0, c)
move _ _ (x, y, a, b, c) = (x, y, a, b, c)

--         wall dimens   state      generated frame
toFrame :: (Int, Int) -> MyState -> ListFrame
toFrame (xdim, ydim) (x', y',a',b',_) =
  ListFrame $
    map
      ( \y ->
          map
            (\x ->
              if x == x' && y == y' && a' == 1 && b' == 1 then Pixel 0xff 0xff 0xff
              else if x == (x' + 1) && y == y' && a' == 1 && b' == 1 then Pixel 0xff 0xff 0xff
              else if x == (x' + 2) && y == y' && a' == 1 && b' == 1 then Pixel 0xff 0xff 0xff
              else if x == x' && y == y' && a' == 1 && b' == 0 then Pixel 0xff 0xff 0xff
              else if x == x' && y == (y' + 1) && a' == 1 && b' == 0 then Pixel 0xff 0xff 0xff
              else if x == x' && y == (y' + 2) && a' == 1 && b' == 0 then Pixel 0xff 0xff 0xff
              else if x == x' && y == y' && a' == 0 && b' == 1 then Pixel 0xff 0x00 0x00
              else if x == (x' + 1) && y == y' && a' == 0 && b' == 1 then Pixel 0xff 0x00 0x00
              else if x == (x' + 2) && y == y' && a' == 0 && b' == 1 then Pixel 0xff 0x00 0x00
              else if x == x' && y == y' && a' == 0 && b' == 0 then Pixel 0xff 0x00 0x00
              else if x == x' && y == (y' + 1) && a' == 0 && b' == 0 then Pixel 0xff 0x00 0x00
              else if x == x' && y == (y' + 2) && a' == 0 && b' == 0 then Pixel 0xff 0x00 0x00 else Pixel 0 0 0)
            [0 .. xdim - 1]
      )
      [0 .. ydim - 1]

--           input events      state      frame       new state
eventTest :: [Event String] -> MyState -> (ListFrame, MyState)
eventTest events state = (toFrame dim state', state')
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


-- END MAIN FUNCTIONALITY

-- BEGIN BATTLESHIP
one :: [[[Int]]]
one = [[[18,2],[18,3],[18,4],[18,5],[18,6]],
       [[11,1],[11,2],[11,3],[11,4]],
       [[13,4],[14,4],[15,4]],
       [[10,9],[11,9],[12,9]],
       [[18,10],[19,10]]]

two :: [[[Int]]]
two = [[[16,3],[17,3],[18,3],[19,3]],
       [[16,7],[16,8],[16,9],[16,10]],
       [[10,3],[11,3],[12,3]],
       [[11,8],[12,8],[13,8]],
       [[13,1],[14,1]]]

three :: [[[Int]]]
three = [[[14,6],[15,6],[16,6],[17,6]],
         [[11,1],[12,1],[13,1]],
         [[19,8],[19,9],[19,10]],
         [[15,1],[15,2]],
         [[10,6],[10,7]]]


-- 0 for x dimension (waagerecht)
-- 1 for y dimension (senkrecht)
type Rotation = Int
type ShipLength = Int

newShip :: ShipLength -> Rotation -> MyState -> [[Int]]
newShip 1 _ (x, y, a, b, c) = [[x,y,a,b,c]]
newShip n 0 (x, y, a, b, c) | (x + n) < 20 = [x + n - 1, y, a, b, c] : newShip (n-1) 0 (x,y, a, b, c)
                            | otherwise = error "ship out of map"
newShip n 1 (x, y, a, b, c) | (y + n) < 11 = [x, y + n - 1, a, b, c] : newShip (n-1) 1 (x,y, a, b, c)
                            | otherwise = error "ship out of map"
newShip _ _ _ = error "mhh something went wrong"

-- if ("Pressed", "SPACE") then rotation n

rotation :: Rotation -> Rotation
rotation 0 = 1
rotation 1 = 0
rotation _ = error "mhh something went wrong"

-- BEGIN CONFIGURATION

ip :: String -- IP address of wall/emulator
ip = "134.28.70.51" -- "127.0.0.1"

delay :: Int -- delay between frames in microseconds
delay = 33000

initState :: MyState -- initial state
initState = (10, 1, 1, 1, 0)

-- END CONFIGURATION

main :: IO ()
main = do
  window <- showSDLControlWindow
  Sock.withSocketsDo $
    runMate
      (Config (fromJust $ parseAddress ip) 1337 dim (Just delay) True [sdlKeyEventProvider, sdlJoystickEventProvider])
      eventTest
      initState
  destroySDLControlWindow window
