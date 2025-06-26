-- Control.Monad
-- replicateM
import Control.Monad (replicateM)
-- Data.Set
-- Set
import Data.Set (Set)
import Data.Set qualified as Set

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

solver :: Int -> [Int] -> [Int]
solver n a = map sum $ chunksOf 7 a

main :: IO ()
main = do
  n <- getInt
  a <- getIntArray
  putStrLn $ unwords $ map show $ solver n a

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