import Data.Char (digitToInt)

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  print $ solver (a + b)

solver :: Int -> Int
solver x = head [i | i <- [0..9], not (i `elem` digits)]
  where
    digits = map digitToInt (show x)