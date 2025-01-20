main :: IO ()
main = do
  [n, x, y, z] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solve x y z

solve :: Int -> Int -> Int -> String
solve x y z 
  | (y < z) && (z < x) = "Yes"
  | (x < z) && (z < y) = "Yes"
  | otherwise = "No"
