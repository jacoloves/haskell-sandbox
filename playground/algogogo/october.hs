import Control.Monad (forM_, replicateM)
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc421a


abc421a :: IO()
abc421a = do
    n <- getInt
    regidents <- replicateM n getStr
    [xStr, y] <- getStrArray
    let x = read xStr :: Int
    let r = checkD regidents x y
    putStrLn r

checkD :: [String] -> Int -> String -> String
checkD regidents x y = 
    let actualResident = regidents !! (x - 1)
    in if actualResident == y 
        then "Yes" 
        else "No"

abc424a :: IO()
abc424a = do
    [a, b, c] <- getIntArray
    let ans = if a == b || b == c || c == a then "Yes" else "No"
    putStrLn ans

abc425a :: IO()
abc425a = do
    n <- getInt
    let r = f n
    print r

f :: Int -> Int
f n = sum [f2 i | i <- [1..n] ]

f2 :: Int -> Int
f2 i = (-1) ^ i * i ^ 3
    


hello :: IO ()
hello = do
  print "hello"

-- readt String NLines
getNLines :: Int -> IO [String]
getNLines n = replicateM n getLine

-- read double Int score
readDoubleIntScore :: Int -> IO [(Int, Int)]
readDoubleIntScore n = replicateM n $ do
  [x, y] <- map read . words <$> getLine
  return (x, y)

-- read String Array
getStrArray :: IO [String]
getStrArray = words <$> getLine

-- read Int
getInt :: IO Int
getInt = readLn :: IO Int

-- read String
getStr :: IO String
getStr = getLine

-- read Int Array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double
