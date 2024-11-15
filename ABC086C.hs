import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn :: IO Int
  plans <- replicateM n $ do
    [t, x, y] <- map read . words <$> getLine
    return [t, x, y]
  putStrLn $ if isPlanPossible plans then "Yes" else "No"

isPlanPossible :: [[Int]] -> Bool
isPlanPossible = go (0, 0, 0)
  where
    go _ [] = True
    go (t0, x0, y0) ([t, x, y] : rest)
      | dt >= dist && even (dt - dist) = go (t, x, y) rest
      | otherwise = False
      where
        dt = t - t0
        dist = abs (x - x0) + abs (y - y0)