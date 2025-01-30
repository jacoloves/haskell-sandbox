main :: IO()
main = do
  _ <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solver a 


solver :: [Int] -> String
solver a = unwords . map show $ zipWith (*) a (tail a)
