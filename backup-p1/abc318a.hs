-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> Int -> Int
solver n m p
  | n < m = 0
  | otherwise = length [day | day <- [m, m + p .. n]]


main :: IO ()
main = do
  [n,m,p] <- getIntArray
  print $ solver n m p

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