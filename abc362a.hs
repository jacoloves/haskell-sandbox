main :: IO ()
main = do
  [r,g,b] <- map read . words <$> getLine :: IO [Int]
  c <- getLine
  print $ funcA r g b c

funcA :: Int -> Int -> Int -> String -> Int
funcA r g b c
  | c == "Red" = min g b
  | c == "Green" = min r b
  | otherwise = min r g

