main :: IO ()
main = do
  [a, b, d] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solver a b d

solver :: Int -> Int -> Int -> String
solver a b d = unwords . map show $ takeWhile (<= b) [a, a + d ..]