-- Control.Monad
-- replicateM
import Control.Monad (replicateM)
-- Data.Set
-- Set
import Data.Set (Set)
import Data.Set qualified as Set

isValidRange :: Int -> Bool
isValidRange x = x >= 100 && x <= 675 && x `mod` 25 == 0

isMonotonicallyIncreasing :: [Int] -> String
isMonotonicallyIncreasing [] = "Yes"
isMonotonicallyIncreasing [_] = "Yes"
isMonotonicallyIncreasing (x:y:xs)
  | (isValidRange x) && (isValidRange y) && x < y = isMonotonicallyIncreasing (y:xs)
  | otherwise = "No"
solver :: [Int] -> String
solver s = isMonotonicallyIncreasing s

main :: IO ()
main = do
  s <- getIntArray
  putStrLn $ solver s

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

-- read Int array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double