import GHC.OldList (sort)

main :: IO ()
main = do
  n <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  print $ aliceBobDifference a

aliceBobDifference :: [Int] -> Int
aliceBobDifference a =
  let sorted = reverse $ sort a
      (alice, bob) = foldl (\(alice, bob) (i, x) -> if even i then (alice + x, bob) else (alice, bob + x)) (0, 0) (zip [0 ..] sorted)
   in alice - bob