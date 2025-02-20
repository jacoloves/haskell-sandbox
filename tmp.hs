-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: [(Int, Int)] -> String
solver scores =
  let (t, a) = foldl (\(t, a) (x, y) -> (t + x, a + y)) (0, 0) scores
  in if t > a
    then "Takahashi"
    else if t < a
      then "Aoki"
      else "Draw"

main :: IO ()
main = do
  n <- readLn :: IO Int
  scores <- readDoubleIntScore n
  putStrLn $ solver scores

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