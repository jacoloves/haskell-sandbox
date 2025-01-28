main :: IO()
main = do
  n <- readLn :: IO Int
  putStrLn $ solver n 



solver :: Int -> String
solver n = concatMap (\i -> if i `mod` 3 == 0 then "x" else "o") [1..n]