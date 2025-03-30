-- Control.Monad
-- replicateM
import Control.Monad (replicateM)
-- Data.Set
-- Set
import Data.Set (Set)
import Data.Set qualified as Set

solver :: Int -> Int -> [Int] -> Int
solver p q scores
  | p > minimumScore = minimumScore
  | otherwise = p
  where
    minimumScore = minimum $ map (+ q) scores

main :: IO ()
main = do
  [_, p, q] <- getIntArray
  scores <- getIntArray
  print $ solver p q scores

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