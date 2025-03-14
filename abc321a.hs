-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver ::  Int -> String
solver n = if is321Like (show n) then "Yes" else "No"
  where
    is321Like :: String -> Bool
    is321Like [x] = True
    is321Like (x:y:xs) = x > y && is321Like (y:xs)
    is321Like _ = False

main :: IO ()
main = do
  n <- getInt
  putStrLn $ solver n

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