main :: IO ()
main = do
  n <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  print $ countDivisions a

countDivisions :: [Int] -> Int
countDivisions = minimum . map countDivisionsForOne

countDivisionsForOne :: Int -> Int
countDivisionsForOne x
  | even x = 1 + countDivisionsForOne (x `div` 2)
  | otherwise = 0