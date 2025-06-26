-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> [(Int, Int)] -> Int
solver s k pq_list =
  let total = sum [p * q | (p, q) <- pq_list]
   in if total >= s then total else (total + k)

main :: IO ()
main = do
  [n, s, k] <- getIntArray
  pq_list <- readDoubleIntScore n
  print $ solver s k pq_list

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