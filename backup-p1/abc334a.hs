-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> Int -> String
solver b g 
  | b > g = "Bat"
  | otherwise = "Glove"


main :: IO ()
main = do
  [b, g] <- getIntArray
  putStrLn $ solver b g

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

-- read Int array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]