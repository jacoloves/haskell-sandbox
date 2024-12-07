main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ abc368A a n k

abc368A :: [Int] -> Int -> Int -> String
abc368A a n k = testA ans
 where
  a' = take (n - k) a
  a'' = drop (n - k) a
  ans = a'' ++ a'

testA :: [Int] -> String
testA ans = unwords $ map show ans
