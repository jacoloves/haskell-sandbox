main :: IO ()
main = do
  [n, c] <- map read . words <$> getLine :: IO [Int]
  t <- map read . words <$> getLine :: IO [Int]
  print $ solve c t

solve :: Int -> [Int] -> Int
solve c times = go (-c) 0 times
 where
  go _ count [] = count
  go last count (x : xs)
    | x - last >= c = go x (count + 1) xs
    | otherwise = go last count xs
