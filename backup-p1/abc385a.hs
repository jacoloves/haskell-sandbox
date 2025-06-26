main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ funcA a b c

funcA :: Int -> Int -> Int -> String
funcA a b c
  | a == b && b == c = "Yes"
  | (a + b) == c = "Yes"
  | (a + c) == b = "Yes"
  | (b + c) == a = "Yes"
  | otherwise = "No"
