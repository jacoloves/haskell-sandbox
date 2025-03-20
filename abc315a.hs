-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

-- input: aaaaabbbbbbccccceeeee
-- output: bbbbbbccccc
-- remove a, i, u, e, o
solver :: String -> String
solver [] = []
solver (x : xs)
  | x `elem` "aiueo" = solver xs
  | otherwise = x : solver xs

main :: IO ()
main = do
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

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double