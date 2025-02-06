
main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ solver n

solver :: Int -> String
solver n = concatMap show $ take (2 * n + 1) $ cycle [1, 0]