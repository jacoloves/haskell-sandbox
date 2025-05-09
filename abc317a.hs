-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> [Int] -> Int
solver h x medicines = length (takeWhile (< x - h) medicines) + 1

main :: IO ()
main = do
  [_, h, x] <- getIntArray
  medicines <- getIntArray
  print $ solver h x medicines

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