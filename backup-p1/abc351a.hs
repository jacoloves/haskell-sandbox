main :: IO()
main = do
  a <- map read . words <$> getLine :: IO [Int]
  b <- map read . words <$> getLine :: IO [Int]
  print $ solve (sum a) (sum b)

solve :: Int -> Int -> Int
solve a b = (a - b + 1)
