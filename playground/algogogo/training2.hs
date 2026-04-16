import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get (getInt16be, remaining)
import Data.Char (digitToInt, isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (elemIndex, group, isSuffixOf, minimumBy, nub, permutations, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set

main :: IO ()
main = do
  training11

training11 :: IO ()
training11 = do
  [n, x, t] <- getIntArray
  let divSeed = n `div` x

  if n `mod` x /= 0
    then print $ (divSeed + 1) * t
    else print $ divSeed * t

training10 :: IO ()
training10 = do
  n <- getInt
  let ans = n `div` 2

  if even n
    then print ans
    else print (ans + 1)

training09 :: IO ()
training09 = do
  n <- getInt

  let tmp1 = n `mod` 1000

  if tmp1 == 0
    then print 0
    else print (1000 - tmp1)

training08 :: IO ()
training08 = do
  [a, b, t] <- getIntArray

  let ans = b * (t `div` a)

  print ans

training07 :: IO ()
training07 = do
  [a, p] <- getIntArray

  let pA = a * 3
      ans = (pA + p) `div` 2

  print ans

training06 :: IO ()
training06 = do
  rgb <- getIntArray

  let rgb_str = concatMap show rgb
      rgb_int = read rgb_str :: Int

  if rgb_int `mod` 4 == 0
    then putStrLn "YES"
    else putStrLn "NO"

training05 :: IO ()
training05 = do
  x <- getInt
  a <- getInt
  b <- getInt

  let ans = (x - a) `mod` b

  print ans

training04Improv :: IO ()
training04Improv = do
  n <- getInt
  a <- getInt

  let ans
        | n `mod` 500 > a = "No"
        | otherwise = "Yes"

  putStrLn ans

training04 :: IO ()
training04 = do
  n <- getInt
  a <- getInt

  let tmp1 = n `mod` 500
  let ans
        | tmp1 > a = "No"
        | otherwise = "Yes"
  putStrLn ans

training03 :: IO ()
training03 = do
  [a, b] <- getIntArray
  let ans
        | b `mod` a == 0 = a + b
        | otherwise = b - a

  print ans

training02 :: IO ()
training02 = do
  [a, b] <- getIntArray

  let ans
        | a >= 13 = b
        | a >= 6 = b `div` 2
        | otherwise = 0

  print ans

training01Improv :: IO ()
training01Improv = do
  [a, b] <- getIntArray

  print $ maximum [a + b, a - b, a * b]

training01 :: IO ()
training01 = do
  [a, b] <- getIntArray

  let plusAB = a + b
      minusAB = a - b
      multiAB = a * b

  print $ maximum [plusAB, minusAB, multiAB]

-- read big Int
getBigInt :: IO Integer
getBigInt = readLn :: IO Integer

-- read big Int Array
getBigIntArray :: IO [Integer]
getBigIntArray = map read . words <$> getLine :: IO [Integer]

-- read Float
getFloat :: IO Float
getFloat = readLn :: IO Float

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
