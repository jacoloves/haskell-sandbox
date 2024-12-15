main :: IO ()
main = do
  r <- readLn :: IO Int
  print $ funcA r

funcA :: Int -> Int
funcA r
  | 1 <= r && r <= 99 = 100 - r
  | 100 <= r && r <= 199 = 200 - r
  | 200 <= r && r <= 299 = 300 - r
  | otherwise = 400 - r
