import Data.IORef (newIORef, readIORef, writeIORef)

dim :: [Int] -- wall dimensions
dim = [30, 12]

-- Field Info
field_ = fieldCreate3D dim
field__ = fieldCreate3D [10, 10]

fieldCreate3D :: [Int] -> [[[Int]]]
-- Similar to 1D Version creates a field (x, y) Position on Board and a third component storing what pixel type.
fieldCreate3D [xdim, ydim] = [[[x, y, 0] | y <- [0..ydim]] | x <- [0..xdim]]

getInfoField3D :: [[[Int]]] -> [Int] -> [Int]
getInfoField3D field [x, y] = [x, y, ([ pI | ys <- field, [x', y', pI] <- ys, x' == x, y' == y] !! 0)]

writeInfoField3D :: [[[Int]]] -> [Int] -> [[[Int]]]
writeInfoField3D field [x, y, info] = [ ([ if (x' == x && y' == y) then [x, y, info] else [x', y', info']  | [x', y', info'] <- ys]) | ys <- field]

--[ (x, y, info) | (x', y', info') <- ys, x' == x, y' == y]

-- Return Int, -1 == Space Already Choosen, 0 == Miss, 1 == Hit
fieldAttackPosition :: [[[Int]]] -> [Int] -> Int
fieldAttackPosition field [x, y] | getInfoList(getInfoField3D field [x, y]) == 1 = 1
                                 | getInfoList(getInfoField3D field [x, y]) == 0 = 0
                                 | otherwise = -1

field3DToPixelsField :: [[[Int]]] -> [[Int]]
field3DToPixelsField field = [ x | ys <- field, x <- ys]


-- fieldInsertShip field [x:xs] amount | amount > 0 = (fieldInsertShip (writeInfoField3D field [(x !! 0), (x !! 1), 1]) xs (amount-1))
                                    -- | otherwise = field


-- Tuple Functions
getInfoList :: [Int] -> Int
getInfoList [x, y, info] = info

getXList :: [Int] -> Int
getXList [x, y, info] = x

getYList :: [Int] -> Int
getYList [x, y, info] = y

getXYList :: [Int] -> [Int]
getXYList [x, y, info] = [x, y]

main :: IO ()
main = do
    cat <- newIORef (3 :: Int)
    readIORef cat >>= print
    -- putStrLn (show (fieldCreate3D dim))
