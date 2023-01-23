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

getKeyDataTuples keyState =
  map (\(k, t) -> ("Pressed", k, t)) (pressed keyState)
    ++ map (\(k, d) -> ("Held", k, d)) (held keyState)
    ++ map (\(k, t) -> ("Released", k, t)) (released keyState)

getButtonDataTuples buttonState =
  map (\(k, t) -> ("Pressed", k, t)) (pressedB buttonState)
    ++ map (\(k, d) -> ("Held", k, d)) (heldB buttonState)
    ++ map (\(k, t) -> ("Released", k, t)) (releasedB buttonState)




-- BEGIN MAIN FUNCTIONALITY

--         wall dimens   state      generated frame
toFrame :: (Int, Int) -> MyState -> ListFrame
toFrame (xdim, ydim) (x', y', cursorColor, mode, drawFrame) =
  ListFrame $
    map
      ( \y ->
          map
            (\x -> 
            if x == x' && y == y' then Pixel 0xFF 0x69 0xB4 
            else if x == (x'+1) && y == (y'+1) then Pixel 0xFF 0xA5 0x0
            else if x == (x') && y == (y'+1) then Pixel 0xFF 0xA5 0x0
            else if x == (x') && y == (y'-1) then Pixel 0xFF 0xA5 0x0
            else if x == (x'+1) && y == (y') then Pixel 0xFF 0xA5 0x0
            else if x == (x'-1) && y == (y') then Pixel 0xFF 0xA5 0x0
            else if x == (x'-1) && y == (y'+1) then Pixel 0xFF 0xA5 0x0
            else if x == (x'-1) && y == (y'-1) then Pixel 0xFF 0xA5 0x0
            else if x == (x'+1) && y == (y'-1) then Pixel 0xFF 0xA5 0x0
            else Pixel 0 200 0)
            [0 .. xdim - 1]
      )
      [0 .. ydim - 1]

-- 0xFF 0x69 0xB4
toFrameList :: (Int, Int) -> [[Int]] -> MyState -> ListFrame
toFrameList (xdim, ydim) pixels (x, y, cursorColor, cursorMode, frameMode) = case frameMode of
  0 -> ListFrame $ [ ([ if (any (==True) [True | [x',y', infoPixel] <- pixels, x' == x, y' == y]) then (pixelType (getInfoPixel pixels [x, y])) else (pixelType 0)  | x <- [0 .. xdim - 1]]) | y <- [0 .. ydim - 1]]
      where
        pixelType info = case info of
          0 -> (Pixel 115 241 255) -- Blue Sea
          1 -> (Pixel 255 0 0) -- Red
          2 -> (Pixel 120 120 120) -- Grey
          3 -> (Pixel 255 132 0) -- Orange
          4 -> (Pixel 0 255 98) -- Green
  1 -> toFrame dim (x, y, cursorMode, frameMode)


getInfoPixel :: [[Int]] -> [Int] -> Int
getInfoPixel pixels [x, y] = [infoPixel | [x', y', infoPixel] <- pixels, x' == x, y' == y] !! 0

--           input events      state      frame       new state
eventMain :: [Event String] -> MyState -> (ListFrame, MyState)
eventMain events (x, y, cursorColor, mode, frameMode) = ((toFrameList dim helloTextPixel state'), state')
                where
                  state'  | length(events) > 0 = ([ trace (show (getKeyDataTuples (read ev :: KeyState))) (x, y, mode, 0)  | (Event mod ev) <- events, mod == "SDL_KEY_DATA"] !! 0)
                          | otherwise          = (x, y, mode, frameMode)

                          -- getKeyEventThing(getKeyDataTuples (read ev :: KeyState)) == "W"
                          -- getKeyEventThing(getKeyDataTuples (read ev :: KeyState)) == "UP"

getKeyEventThing :: [([Char], String, Integer)] -> String
getKeyEventThing [(action, key, duration)] = key
getKeyEventThing _ = "Nothing"

testKey = [("Held","L-SHIFT",339)]
testKeyComplicated = [("Released","L-CTRL",16944),("Released","C",16944)]

                    -- map ( \acc (Event mod ev) -> case mod of
                                       -- "SDL_KEY_DATA" ->
                                        --  foldl (flip (move dim)) acc (getKeyDataTuples (read ev :: KeyState))
                                   -- ) events

                           -- , (getKeyDataTuples (read ev :: KeyState))


-- Text Hello as Pixels
helloTextPixel = [[0, 0, 2], [0, 1, 2], [0, 2, 2], [1, 1, 2], [2, 0, 2], [2, 1, 2], [2, 2, 2], [4, 0, 1], [4, 1, 1], [4, 2, 1], [4, 3, 1], [4, 4, 1], [5, 0, 1], [5, 2, 1], [5, 4, 1], [7, 0, 3], [7, 1, 3], [7, 2, 3], [8, 2, 3], [10, 0, 3], [10, 1, 3], [10, 2, 3], [11, 2, 3], [13, 0, 4], [14, 0, 4], [15, 0, 4], [13, 2, 4], [14, 2, 4], [15, 2, 4], [13, 1, 4], [15, 1, 4]]

-- END MAIN FUNCTIONALITY
-- BEGIN CONFIGURATION

ip :: String -- IP address of wall/emulator
ip = "134.28.70.51" -- "127.0.0.1"

delay :: Int -- delay between frames in microseconds
delay = 33000

-- Test Variable
printUseless = putStrLn ((show 3))


type MyState = (Int, Int, Int, Int, Int)
-- (x, y,.. which represent cursors position
-- Color Cursor
-- Cursor Mode
-- Gameplay Mode (0-8) (0 == Main Menu/Nothing, 1 == Player One Place Ships, 2 == Player Two Place Ships, 3-4 == Player One Attack, 5-6 == Player Two Attack, 7 == Player One Won, 8 == Player Two won)

initState :: MyState -- initial state
initState = (0, 0, 1, 1, 0) -- For Info see type above.


-- END CONFIGURATION

main :: IO ()
main = do
  window <- showSDLControlWindow
  Sock.withSocketsDo $
    runMate
      (Config (fromJust $ parseAddress ip) 1337 dim (Just delay) True [sdlKeyEventProvider, sdlJoystickEventProvider])
      eventMain
      initState
  destroySDLControlWindow window
