main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  print $ abc369A a b

abc369A :: Int -> Int -> Int
abc369A a b
  | a == b = 1
  | even (a + b) = 3
  | otherwise = 2
