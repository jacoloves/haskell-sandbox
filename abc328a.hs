-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> [Int] -> Int
solver x scores = sum $ filter (\s -> s <= x) scores

main :: IO ()
main = do
  [_, x] <- getIntArray
  scores <- getIntArray
  print $ solver x scores

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