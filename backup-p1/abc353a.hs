import Data.List (findIndex)

main :: IO ()
main = do
  _ <- readLn :: IO Int
  h <- map read . words <$> getLine :: IO [Int]
  print $ solve h

solve :: [Int] -> Int
solve [] = -1
solve (x : xs) = maybe (-1) (+ 2) $ findIndex (> x) xs