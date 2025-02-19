-- Control.Monad
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
  scores <- replicateM n $ do
    [x, y] <- map read . words <$> getLine
    return (x, y)
  putStrLn $ solver scores

-- read String Array
getStrArray :: IO [String]
getStrArray = words <$> getLine