main :: IO ()
main = do
  [_, m] <- map read . words <$> getLine :: IO [Int]
  h <- map read . words <$> getLine
  print $ solve m h

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve m (x : xs)
  | m >= x = 1 + solve (m - x) xs
  | otherwise = 0
