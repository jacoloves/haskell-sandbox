-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: String -> String
solver s = if any (\(x, y) -> (x == 'a' && y== 'b') || (x == 'b' && y == 'a')) (zip s (tail s))
  then "Yes"
  else "No"

main :: IO ()
main = do
  _ <- getInt
  s <- getStr
  putStrLn $ solver s

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