main :: IO ()
main = do
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solver a

solver :: [Int] -> String
solver [a1, a2, a3]
  | a1 * a2 == a3 = "Yes"
  | a1 * a3 == a2 = "Yes"
  | a3 * a2 == a1 = "Yes"
  | otherwise = "No"
