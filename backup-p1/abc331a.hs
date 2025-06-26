-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> Int -> Int -> Int -> String
solver m d y mm dd
  | mm == m && dd == d = unwords $ map show [y + 1, 1, 1]
  | dd == d = unwords $ map show [y, mm + 1, 1]
  | otherwise = unwords $ map show [y, mm, dd + 1]

main :: IO ()
main = do
  [m, d] <- getIntArray
  [y, mm, dd] <- getIntArray
  putStrLn $ solver m d y mm dd

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