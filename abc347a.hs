import Data.List (sort)

main :: IO()
main = do
  [_, k] <- map read . words <$> getLine :: IO [Int]
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solver k a 


solver :: Int -> [Int] -> String
solver k a = unwords . map show . sort $ map (`div` k) (filter (\x -> x `mod` k == 0) a)
