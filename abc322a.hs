-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: String -> Int
solver s = findABC s 0
  where
    findABC :: String -> Int -> Int
    findABC str i
      | length str < 3 = -1
      | take 3 str == "ABC" = i + 1
      | otherwise = findABC (tail str) (i + 1)

main :: IO ()
main = do
  _ <- getInt
  s <- getStr
  print $ solver s

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