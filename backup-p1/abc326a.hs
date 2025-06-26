-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> String
solver a b
  | a < b = upStair a b
  | otherwise = downStair a b

upStair :: Int -> Int -> String
upStair a b
  | (b - a) >= 3 = "No"
  | otherwise = "Yes"

downStair :: Int -> Int -> String
downStair a b
  | (a - b) >= 4 = "No"
  | otherwise = "Yes"

main :: IO ()
main = do
  [a, b] <- getIntArray
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