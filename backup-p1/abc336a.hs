-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: Int -> String
solver n = "L" ++ replicate n 'o' ++ "ng"


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