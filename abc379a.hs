main :: IO ()
main = do
  n <- readLn :: IO Int
  let (a, b, c) = extractDigits n
  let bca = b * 100 + c * 10 + a
  let cab = c * 100 + a * 10 + b
  putStrLn $ show bca ++ " " ++ show cab

extractDigits :: Int -> (Int, Int, Int)
extractDigits n = (a, b, c)
  where
    a = n `div` 100
    b = (n `div` 10) `mod` 10
    c = n `mod` 10