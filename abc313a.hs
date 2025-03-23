-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: [Int] -> Int
solver [] = 0
solver (p : ps) = judge p (maximum ps)
  where
    judge :: Int -> Int -> Int
    judge pp q
      | pp > q = 0
      | otherwise = (q - pp) + 1

main :: IO ()
main = do
  _ <- getInt
  ps <- getIntArray
  print $ solver ps

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