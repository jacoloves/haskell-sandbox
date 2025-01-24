main :: IO()
main = do
  _ <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  print $ solve a



solve :: [Int] -> Int
solve a = -sum a
