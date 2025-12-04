import Control.Monad (forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (group, isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Data.Set qualified as Set

main :: IO ()
main = do
  training014

training014 :: IO ()
training014 = do
  x <- getInt
  let ans = head [n | n <- [x ..], isPrime n]
  print ans

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\d -> n `mod` d /= 0) [3, 5 .. limit]
  where
    limit = floor $ sqrt $ fromIntegral n

training013 :: IO ()
training013 = do
  s <- getInt
  let ans = findCycle s
  print ans

findCycle :: Int -> Int
findCycle s = go s Set.empty 1
  where
    go current seen index
      | current `Set.member` seen = index
      | otherwise = go (f current) (Set.insert current seen) (index + 1)

    f n = if even n then n `div` 2 else 3 * n + 1

training012 :: IO ()
training012 = do
  [n, m, x] <- getIntArray
  tollGates <- getIntArray

  let leftCost = length $ filter (< x) tollGates
      rightCost = length $ filter (> x) tollGates

  print $ min leftCost rightCost

training011 :: IO ()
training011 = do
  s <- getStr
  let lengths = findAllACGTLengths s
  print $ if null lengths then 0 else maximum lengths

findAllACGTLengths :: String -> [Int]
findAllACGTLengths s = go s 0
  where
    go [] currentLen = [currentLen | currentLen > 0]
    go (c : cs) currentLen
      | isACGT c = go cs (currentLen + 1)
      | currentLen > 0 = currentLen : go cs 0
      | otherwise = go cs 0

    isACGT c = c `elem` "ACGT"

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