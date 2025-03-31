-- Control.Monad
-- replicateM
import Control.Monad (replicateM)
-- Data.Set
-- Set
import Data.Set (Set)
import Data.Set qualified as Set

solver :: Int -> Int -> String
solver a b
  | a == 1 && b == 2 = "Yes"
  | a == 2 && b == 3 = "Yes"
  | a == 4 && b == 5 = "Yes"
  | a == 5 && b == 6 = "Yes"
  | a == 7 && b == 8 = "Yes"
  | a == 8 && b == 9 = "Yes"
  | otherwise = "No"

main :: IO ()
main = do
  [a,b] <- getIntArray
  putStrLn $ solver a b

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