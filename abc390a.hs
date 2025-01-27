main :: IO ()
main = do
  a <- map read . words <$> getLine :: IO[Int]
  putStrLn $ solve a

solve :: [Int] -> String
solve a
  | a == [1,2,4,3,5] = "Yes"
  | a == [2,1,3,4,5] = "Yes"
  | a == [1,3,2,4,5] = "Yes"
  | a == [1,2,3,5,4] = "Yes"
  | otherwise = "No"
