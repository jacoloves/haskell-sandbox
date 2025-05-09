-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: [Int] -> String
solver a = if all (\(x, y) -> x < y) (zip a (tail a))
  then "Yes"
  else "No"

main :: IO ()
main = do
  _ <- getInt
  a <- getIntArray
  putStrLn $ solver a 

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