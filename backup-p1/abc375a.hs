main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  print $ funcCount s

funcCount :: String -> Int
funcCount s = go s 0
  where
    go (x : y : z : xs) count
      | x == '#' && y == '.' && z == '#' = go (y : z : xs) (count + 1)
      | otherwise = go (y : z : xs) count
    go _ count = count
