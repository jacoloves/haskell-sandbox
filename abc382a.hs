main :: IO ()
main = do
  [n, d] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  print $ solve d s

solve :: Int -> String -> Int
solve d s = d + length (filter (== '.') s)
